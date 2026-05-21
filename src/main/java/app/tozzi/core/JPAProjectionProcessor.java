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
import lombok.NonNull;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.query.QueryUtils;

import java.lang.reflect.Field;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

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

        var selectionsAndJoins = loadSelectionsFromFields(fields, root, projectableFields, throwsIfNotExists, overrideJoins, overrideJoinTypes);
        var selections = selectionsAndJoins.getLeft();
        var joins = selectionsAndJoins.getRight();
        return loadCompleteSelections(selections, joins, root, entityClass, idFields);
    }

    private static Pair<List<Selection<?>>, Map<String, Join<?, ?>>> loadSelectionsFromFields(
            List<String> fields,
            Root<?> root,
            Map<String, Pair<Projectable, Field>> projectableFields,
            boolean throwsIfNotExists,
            boolean overrideJoins,
            Map<String, JoinType> overrideJoinTypes) {

        var joins = new LinkedHashMap<String, Join<?, ?>>();

        var selections = fields.stream()
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

                    if (!overrideJoins) {
                        buildJoins(key, root, joins, null);
                    } else if (overrideJoinTypes != null && !overrideJoinTypes.isEmpty()) {
                        buildJoins(key, root, joins, overrideJoinTypes);
                    }

                    var lastDotIndex = key.lastIndexOf('.');
                    var parentPath = lastDotIndex == -1 ? null : key.substring(0, lastDotIndex);
                    var leafSegment = lastDotIndex == -1 ? key : key.substring(lastDotIndex + 1);

                    if (parentPath != null && joins.containsKey(parentPath)) {
                        return (Selection<?>) joins.get(parentPath).get(leafSegment).alias(key);
                    }
                    return (Selection<?>) JPASearchUtils.getPath(root, key).alias(key);
                })
                .collect(Collectors.toCollection(ArrayList<Selection<?>>::new));

        return Pair.of(selections, joins);
    }

    private static void buildJoins(String key, Root<?> root, Map<String, Join<?, ?>> joins, Map<String, JoinType> overrideJoinTypes) {
        var segments = key.split("\\.");
        if (segments.length < 2) {
            return;
        }

        IntStream.range(0, segments.length - 1).forEach(i -> {
            var segmentPath = String.join(".", Arrays.copyOfRange(segments, 0, i + 1));
            if (joins.containsKey(segmentPath)) {
                return;
            }

            var joinType = overrideJoinTypes != null
                    ? overrideJoinTypes.getOrDefault(segmentPath, JoinType.LEFT)
                    : JoinType.LEFT;

            if (i == 0) {
                joins.put(segmentPath, root.join(segments[i], joinType));
            } else {
                var parentPath = String.join(".", Arrays.copyOfRange(segments, 0, i));
                joins.put(segmentPath, joins.get(parentPath).join(segments[i], joinType));
            }
        });
    }

    private static List<Selection<?>> loadCompleteSelections(List<Selection<?>> selections, Map<String, Join<?, ?>> joins, Root<?> root, Class<?> entityClass, Map<Class<?>, Map<String, Field>> idFields) {
        var existingAliases = selections.stream()
                .map(Selection::getAlias)
                .collect(Collectors.toSet());

        idFields.forEach((currentClass, value) -> value.keySet().forEach(idPath -> {
            if (existingAliases.contains(idPath)) {
                return;
            }

            if (currentClass.equals(entityClass)) {
                selections.add(JPASearchUtils.getPath(root, idPath).alias(idPath));
                existingAliases.add(idPath);
            } else {
                var parts = idPath.split("\\.");
                var entityPath = String.join(".", Arrays.copyOf(parts, parts.length - 1));
                var entityPathPrefix = entityPath + ".";

                var hasSelectionForEntity = existingAliases.stream()
                        .anyMatch(alias -> alias.equals(entityPath) || alias.startsWith(entityPathPrefix));

                if (hasSelectionForEntity) {
                    var lastDot = idPath.lastIndexOf('.');
                    var parentPath = idPath.substring(0, lastDot);
                    var leafSegment = idPath.substring(lastDot + 1);

                    var idSelection = joins.containsKey(parentPath)
                            ? joins.get(parentPath).get(leafSegment).alias(idPath)
                            : JPASearchUtils.getPath(root, idPath).alias(idPath);

                    selections.add(idSelection);
                    existingAliases.add(idPath);
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

        SelectionMetadata(String alias, Class<?> rootClass) {
            this.pathParts = alias.split("\\.");
            int len = pathParts.length;
            this.fields = new Field[len];
            this.isCollection = new boolean[len];
            this.nextTypes = new Class<?>[len];

            var currentClass = rootClass;
            for (int i = 0; i < len; i++) {
                var f = FieldUtils.getField(currentClass, pathParts[i], true);
                if (f == null) {
                    throw new JPASearchException(new NoSuchFieldException(
                            "Field '" + pathParts[i] + "' not found in class hierarchy of " + currentClass.getName()));
                }
                this.fields[i] = f;
                this.isCollection[i] = Collection.class.isAssignableFrom(f.getType());
                this.nextTypes[i] = ReflectionUtils.getType(f);
                currentClass = this.nextTypes[i];
            }
        }
    }

    public static List<Map<String, Object>> toMap(List<Tuple> tuple, Class<?> entityClass, List<Selection<?>> selections, Map<Class<?>, Map<String, Field>> idFields) {
        if (tuple == null || tuple.isEmpty()) {
            return Collections.emptyList();
        }

        var metadataCache = selections.stream()
                .collect(Collectors.toMap(
                        Selection::getAlias,
                        s -> new SelectionMetadata(s.getAlias(), entityClass)));

        var map = new LinkedHashMap<CacheKey, Map<String, Object>>();
        tuple.forEach(t -> toMap(t, map, entityClass, selections, idFields, metadataCache));

        return map.entrySet().stream()
                .filter(e -> e.getKey().clazz().equals(entityClass))
                .map(Map.Entry::getValue)
                .toList();
    }

    private static void toMap(Tuple tuple, Map<CacheKey, Map<String, Object>> ids, Class<?> entityClass,
                              List<Selection<?>> selections, Map<Class<?>, Map<String, Field>> idFields,
                              Map<String, SelectionMetadata> metadataCache) {

        var currentEntityContexts = new HashMap<Class<?>, Map<String, Object>>();

        idFields.forEach((currentEntityClass, currentIdMap) -> {
            // IMPORTANT: sort by id-field NAME (key), not by the value's toString.
            // Sorting by value produces false-positive cache hits for compound keys whose
            // values are permutations of each other (e.g. {a=1, b=2} vs {a=2, b=1}),
            // since the underlying HashMap has no stable iteration order.
            var rawIds = currentIdMap.keySet().stream()
                    .sorted()
                    .flatMap(k -> {
                        try {
                            var val = tuple.get(k);
                            return val != null ? java.util.stream.Stream.of(val) : java.util.stream.Stream.empty();
                        } catch (IllegalArgumentException ignored) {
                            return java.util.stream.Stream.empty();
                        }
                    })
                    .toList();

            if (!rawIds.isEmpty()) {
                var entityMap = ids.computeIfAbsent(new CacheKey(currentEntityClass, rawIds), k -> new LinkedHashMap<>());
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
                    if (isCollection) {
                        @SuppressWarnings("unchecked")
                        var coll = (Collection<Object>) currentMap.computeIfAbsent(currentPathName, k -> createCollection(field));
                        if (value != null && !coll.contains(value)) {
                            coll.add(value);
                        }
                    } else {
                        currentMap.put(currentPathName, value);
                    }
                } else {
                    if (isCollection) {
                        @SuppressWarnings("unchecked")
                        var collection = (Collection<Object>) currentMap.computeIfAbsent(currentPathName, k -> createCollection(field));
                        var childMap = currentEntityContexts.get(nextType);

                        if (childMap == null) break;

                        if (collection.stream().noneMatch(e -> e == childMap)) {
                            collection.add(childMap);
                        }
                        currentMap = childMap;
                    } else {
                        if (currentEntityContexts.containsKey(nextType)) {
                            var nextMap = currentEntityContexts.get(nextType);
                            @SuppressWarnings("unchecked")
                            var existing = (Map<String, Object>) currentMap.get(currentPathName);
                            if (existing == null) {
                                currentMap.put(currentPathName, nextMap);
                            }
                            currentMap = existing != null ? existing : nextMap;
                        } else {
                            if (idFields.containsKey(nextType)) break;
                            @SuppressWarnings("unchecked")
                            var existing = (Map<String, Object>) currentMap.get(currentPathName);
                            if (existing == null) {
                                var nextMap = new LinkedHashMap<String, Object>();
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
    }

    private record CacheKey(Class<?> clazz, List<Object> ids) {

        @Override
            public boolean equals(Object o) {
                if (this == o) return true;
                if (!(o instanceof CacheKey other)) return false;
                return Objects.equals(clazz, other.clazz) && Objects.equals(ids, other.ids);
            }

    }

    private static Collection<Object> createCollection(Field field) {
        if (Set.class.isAssignableFrom(field.getType())) return new HashSet<>();
        if (Queue.class.isAssignableFrom(field.getType())) return new LinkedList<>();
        return new ArrayList<>();
    }
}
