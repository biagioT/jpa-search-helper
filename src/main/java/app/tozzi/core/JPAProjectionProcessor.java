package app.tozzi.core;

import app.tozzi.annotation.Projectable;
import app.tozzi.annotation.Searchable;
import app.tozzi.exception.InvalidFieldException;
import app.tozzi.exception.JPASearchException;
import app.tozzi.model.ProjectionDescriptor;
import app.tozzi.model.input.JPASearchInput;
import app.tozzi.util.JPASearchUtils;
import app.tozzi.util.ReflectionUtils;
import jakarta.persistence.Tuple;
import jakarta.persistence.criteria.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NonNull;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.query.QueryUtils;

import java.lang.reflect.Field;
import java.util.*;
import java.util.stream.Collectors;

public class JPAProjectionProcessor {

    public static <E> ProjectionDescriptor getQuery(@NonNull JPASearchInput input, @NonNull Class<?> type, @NonNull Class<E> entityClass,
                                                    @NonNull CriteriaBuilder criteriaBuilder, @NonNull Map<Class<?>, Map<String, Field>> idFields, boolean processSortOptions, Map<String, JoinType> fetchMap,
                                                    Map<String, String> entityFieldMap, Map<String, Pair<Searchable, Field>> searchableFields, boolean overrideJoins, Map<String, JoinType> overrideJoinTypes) {

        if (input.getOptions() == null) {
            throw new JPASearchException("Invalid projection");
        }

        Specification<E> specification = JPASearchCore.specification(
                input.getFilter(),
                ReflectionUtils.getAllSearchableFields(type), fetchMap, entityFieldMap);

        var query = criteriaBuilder.createTupleQuery();
        var root = query.from(entityClass);
        var predicate = specification.toPredicate(root, query, criteriaBuilder);
        var selections = loadSelection(input.getOptions().getSelections(), root, entityClass, ReflectionUtils.getAllProjectableFields(type), idFields, true, overrideJoins, overrideJoinTypes);
        var criteriaQuery = query.multiselect(selections);
        if (predicate != null) {
            criteriaQuery = query.where(predicate);
        }

        if (processSortOptions) {
            var sort = JPASearchCore.loadSort(input.getOptions(), searchableFields, entityFieldMap);
            criteriaQuery = criteriaQuery.orderBy(QueryUtils.toOrders(sort, root, criteriaBuilder));
        }

        return new ProjectionDescriptor(criteriaQuery, selections, input, root);
    }

    public static List<Selection<?>> loadSelection(List<String> fields, Root<?> root, Class<?> entityClass,
                                                   Map<String, Pair<Projectable, Field>> projectableFields, Map<Class<?>, Map<String, Field>> idFields, boolean throwsIfNotExists, boolean overrideJoins, Map<String, JoinType> overrideJoinTypes) {

        if (fields == null || fields.isEmpty()) {
            throw new JPASearchException("Invalid projection");
        }

        if (idFields == null || idFields.isEmpty()) {
            throw new JPASearchException("Invalid entity");
        }

        var selectionsAndJoins = loadSelectionsFromFields(fields, root, entityClass, projectableFields, idFields, throwsIfNotExists, overrideJoins, overrideJoinTypes);
        var selections = selectionsAndJoins.getLeft();
        var joins = selectionsAndJoins.getRight();
        return loadCompleteSelections(selections, joins, root, entityClass, idFields);
    }

    private static Pair<List<Selection<?>>, Map<String, Join<?, ?>>> loadSelectionsFromFields(List<String> fields, Root<?> root, Class<?> entityClass,
                                                                                              Map<String, Pair<Projectable, Field>> projectableFields, Map<Class<?>, Map<String, Field>> idFields, boolean throwsIfNotExists, boolean overrideJoins, Map<String, JoinType> overrideJoinTypes) {
        var joins = new LinkedHashMap<String, Join<?, ?>>();

        return Pair.of(fields.stream()
                .filter(f -> {
                    if (!projectableFields.containsKey(f)) {
                        if (throwsIfNotExists) {
                            throw new InvalidFieldException("Field [" + f + "] does not exist or is not projectable", f);
                        }
                        return false;
                    }
                    return true;
                })
                .map(f -> {
                    var projection = projectableFields.get(f).getLeft();
                    var key = (projection.entityFieldKey() != null && !projection.entityFieldKey().isBlank())
                            ? projection.entityFieldKey()
                            : f;

                    var lastDotIndex = key.lastIndexOf('.');
                    var parentPath = lastDotIndex == -1 ? key : key.substring(0, lastDotIndex);

                    if (!overrideJoins || (overrideJoinTypes != null && overrideJoinTypes.isEmpty())) {
                        idFields.entrySet().stream().filter(e -> !e.getKey().equals(entityClass))
                                .flatMap(e -> e.getValue().keySet().stream())
                                .map(k -> {
                                    var ldi = k.lastIndexOf('.');
                                    var pp = ldi == -1 ? k : k.substring(0, ldi);
                                    return Pair.of(k, pp);
                                })
                                .filter(p -> p.getRight().equals(parentPath))
                                .map(Pair::getLeft)
                                .forEach(k -> {
                                    var path = key.split("\\.");
                                    var completeCurrentPath = new StringBuilder();

                                    for (var i = 0; i < path.length - 1; i++) {
                                        var cp = path[i];
                                        var previousPath = i > 0 ? path[i - 1] : "";
                                        var completePreviousPath = completeCurrentPath.toString();

                                        if (i > 0) {
                                            completeCurrentPath.append(".");
                                        }

                                        completeCurrentPath.append(path[i]);

                                        if (!overrideJoins) {
                                            if (!joins.containsKey(completeCurrentPath.toString())) {
                                                var join = previousPath.isBlank() ? root.join(cp, JoinType.LEFT) : joins.get(completePreviousPath);
                                                joins.put(completeCurrentPath.toString(), previousPath.isBlank() ? join : join.join(cp, JoinType.LEFT));
                                            }

                                        } else if (overrideJoinTypes.containsKey(completeCurrentPath.toString()) && !joins.containsKey(completeCurrentPath.toString())) {
                                            var join = previousPath.isBlank() ? root.join(cp, JoinType.LEFT) : joins.get(completePreviousPath);
                                            joins.put(completeCurrentPath.toString(), previousPath.isBlank() ? join : join.join(cp, overrideJoinTypes.get(completeCurrentPath.toString())));
                                        }
                                    }
                                });
                    }

                    var currentPath = lastDotIndex == -1 ? key : key.substring(lastDotIndex + 1);
                    return joins.containsKey(parentPath) ? joins.get(parentPath).get(currentPath).alias(key) : JPASearchUtils.getPath(root, key).alias(key);

                })
                .collect(Collectors.toCollection(ArrayList<Selection<?>>::new)), joins);
    }

    private static List<Selection<?>> loadCompleteSelections(List<Selection<?>> selections, Map<String, Join<?, ?>> joins, Root<?> root, Class<?> entityClass, Map<Class<?>, Map<String, Field>> idFields) {
        idFields.forEach((currentClass, value) -> value.keySet().forEach(el -> {
            var matchesCurrentEntity = currentClass.equals(entityClass);
            if (matchesCurrentEntity && selections.stream().noneMatch(selection -> selection.getAlias().equals(el))) {
                selections.add(JPASearchUtils.getPath(root, el).alias(el));

            } else if (!matchesCurrentEntity) {
                var parts = el.split("\\.");
                var path = String.join(".", Arrays.copyOf(parts, parts.length - 1));
                var lastDotIndex = el.lastIndexOf('.');
                var parentPath = lastDotIndex == -1 ? el : el.substring(0, lastDotIndex);
                var currentPath = lastDotIndex == -1 ? el : el.substring(lastDotIndex + 1);

                if (selections.stream().anyMatch(s -> s.getAlias().startsWith(path)) && selections.stream().noneMatch(selection -> selection.getAlias().equals(el))) {
                    selections.add(joins.containsKey(parentPath) ? joins.get(parentPath).get(currentPath).alias(el) : JPASearchUtils.getPath(root, el).alias(el));
                }
            }
        }));

        return selections;
    }

    private static class SelectionMetadata {
        final String[] pathParts;
        final Field[] fields;
        final boolean[] isCollection;
        final Class<?>[] nextTypes;

        public SelectionMetadata(String alias, Class<?> rootClass) {
            this.pathParts = alias.split("\\.");
            int len = pathParts.length;
            this.fields = new Field[len];
            this.isCollection = new boolean[len];
            this.nextTypes = new Class<?>[len];

            var currentClass = rootClass;
            for (int i = 0; i < len; i++) {
                try {
                    var f = currentClass.getDeclaredField(pathParts[i]);
                    f.setAccessible(true);
                    this.fields[i] = f;
                    this.isCollection[i] = Collection.class.isAssignableFrom(f.getType());

                    var nextType = ReflectionUtils.getType(f);
                    this.nextTypes[i] = nextType;

                    currentClass = nextType;
                } catch (NoSuchFieldException e) {
                    throw new JPASearchException(e);
                }
            }
        }
    }

    public static List<Map<String, Object>> toMap(List<Tuple> tuple, Class<?> entityClass, List<Selection<?>> selections, Map<Class<?>, Map<String, Field>> idFields) {
        if (tuple == null || tuple.isEmpty()) {
            return Collections.emptyList();
        }

        var metadataCache = new HashMap<String, SelectionMetadata>();
        for (var selection : selections) {
            metadataCache.put(selection.getAlias(), new SelectionMetadata(selection.getAlias(), entityClass));
        }

        // Cache basata su CacheKey (Class + Lista ID) per aggregare le righe
        var map = new LinkedHashMap<CacheKey, Map<String, Object>>();
        tuple.forEach(t -> toMap(t, map, entityClass, selections, idFields, metadataCache));

        return map.entrySet().stream()
                .filter(e -> e.getKey().getClazz().equals(entityClass))
                .map(Map.Entry::getValue)
                .toList();
    }

    private static void toMap(Tuple tuple, Map<CacheKey, Map<String, Object>> ids, Class<?> entityClass,
                              List<Selection<?>> selections, Map<Class<?>, Map<String, Field>> idFields,
                              Map<String, SelectionMetadata> metadataCache) {

        var currentEntityContexts = new HashMap<Class<?>, Map<String, Object>>();

        idFields.forEach((currentEntityClass, currentIdMap) -> {
            var rawIds = new ArrayList<>();
            var hasId = false;

            for (var k : currentIdMap.keySet()) {
                try {
                    var val = tuple.get(k);
                    if (val != null) {
                        rawIds.add(val);
                        hasId = true;
                    }
                } catch (IllegalArgumentException ignored) {
                }
            }

            if (hasId && !rawIds.isEmpty()) {
                var key = new CacheKey(currentEntityClass, rawIds);
                var entityMap = ids.computeIfAbsent(key, k -> new LinkedHashMap<>());
                currentEntityContexts.put(currentEntityClass, entityMap);
            }
        });

        for (var selection : selections) {
            var alias = selection.getAlias();
            var meta = metadataCache.get(alias);

            var value = tuple.get(alias);
            var currentMap = currentEntityContexts.get(entityClass);

            if (currentMap == null) continue;

            for (int i = 0; i < meta.pathParts.length; i++) {
                var currentPathName = meta.pathParts[i];
                var isCollection = meta.isCollection[i];
                var field = meta.fields[i];
                var nextType = meta.nextTypes[i];

                if (i == meta.pathParts.length - 1) {
                    // Foglia
                    if (isCollection) {
                        var coll = (Collection<Object>) currentMap.computeIfAbsent(currentPathName, k -> createCollection(field));
                        // Deduplicazione per valore (String, Long, ecc) usando contains standard
                        if (value != null && !coll.contains(value)) {
                            coll.add(value);
                        }
                    } else {
                        currentMap.put(currentPathName, value);
                    }
                } else {
                    // Nodo Intermedio
                    if (isCollection) {
                        var collection = (Collection<Object>) currentMap.computeIfAbsent(currentPathName, k -> createCollection(field));
                        var childMap = currentEntityContexts.get(nextType);

                        if (childMap != null) {
                            // DEDUPLICAZIONE ROBUSTA: Usa Reference Equality (==)
                            // Poiché childMap viene dalla cache (currentEntityContexts),
                            // se è la stessa entità logica, è garantito che sia lo stesso oggetto Java.
                            // Questo evita problemi con Map.equals() su mappe mutevoli.
                            var alreadyExists = false;
                            for (var existing : collection) {
                                if (existing == childMap) {
                                    alreadyExists = true;
                                    break;
                                }
                            }

                            if (!alreadyExists) {
                                collection.add(childMap);
                            }

                            currentMap = childMap;
                        } else {
                            break;
                        }
                    } else {
                        Map<String, Object> nextMap;
                        if (currentEntityContexts.containsKey(nextType)) {
                            nextMap = currentEntityContexts.get(nextType);
                        } else {
                            // Se non c'è ID, la relazione è null. Stop.
                            if (idFields.containsKey(nextType)) {
                                break;
                            }
                            nextMap = new LinkedHashMap<>();
                        }

                        var existing = (Map<String, Object>) currentMap.get(currentPathName);
                        if (existing == null) {
                            currentMap.put(currentPathName, nextMap);
                            currentMap = nextMap;
                        } else {
                            currentMap = existing;
                        }
                    }
                }
            }
        }
    }

    /**
     * Chiave interna per cache. Sostituisce JPAEntityId.
     */
    @Data
    @AllArgsConstructor
    private static class CacheKey {
        private Class<?> clazz;
        private List<Object> ids;
    }

    private static Collection<Object> createCollection(Field field) {
        if (List.class.isAssignableFrom(field.getType())) {
            return new ArrayList<>();
        }
        if (Set.class.isAssignableFrom(field.getType())) {
            return new HashSet<>();
        }
        if (Queue.class.isAssignableFrom(field.getType())) {
            return new LinkedList<>();
        }
        return new ArrayList<>();
    }
}