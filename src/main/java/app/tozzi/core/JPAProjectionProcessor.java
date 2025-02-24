package app.tozzi.core;

import app.tozzi.annotation.Projectable;
import app.tozzi.annotation.Searchable;
import app.tozzi.exception.InvalidFieldException;
import app.tozzi.exception.JPASearchException;
import app.tozzi.model.JPAEntityId;
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

    private static void toMap(Tuple tuple, Map<ClassID, Map<String, Object>> ids, Class<?> entityClass, List<Selection<?>> selections,
                              Map<Class<?>, Map<String, Field>> idFields) {


        var currentIds = new LinkedHashMap<Class<?>, Pair<JPAEntityId, Map<String, Object>>>();

        idFields.forEach((currentEntityClass, currentIdMap) -> {
            var id = new JPAEntityId();
            id.setIds(new LinkedList<>());
            currentIdMap.keySet().forEach(k -> {
                tuple.getElements().stream().filter(el -> el.getAlias().equals(k)).findAny().ifPresent(t -> {
                    id.getIds().add(tuple.get(k));
                });
            });

            if (!id.getIds().isEmpty()) {
                var classId = new ClassID(currentEntityClass, id);
                ids.putIfAbsent(classId, new LinkedHashMap<>());
                currentIds.put(currentEntityClass, Pair.of(id, ids.get(classId)));
            }
        });

        for (var selection : selections) {
            var path = selection.getAlias().split("\\.");
            var currentClass = entityClass;
            var currentMap = currentIds.get(currentClass).getRight();
            var value = tuple.get(selection.getAlias());

            for (int i = 0; i < path.length; i++) {
                var currentPath = path[i];
                Field field;
                try {
                    field = currentClass.getDeclaredField(currentPath);
                } catch (NoSuchFieldException e) {
                    throw new RuntimeException(e);
                }
                field.setAccessible(true);

                if (i == path.length - 1) {
                    if (Collection.class.isAssignableFrom(field.getType())) {
                        ((Collection<Object>) currentMap.computeIfAbsent(currentPath, k -> createCollection(field))).add(value);

                    } else {
                        currentMap.put(currentPath, value);
                    }

                } else {
                    if (Collection.class.isAssignableFrom(field.getType())) {
                        Collection<Object> collection;
                        var newCollection = false;

                        if (!currentMap.containsKey(currentPath)) {
                            collection = createCollection(field);
                            currentMap.put(currentPath, collection);
                            newCollection = true;

                        } else {
                            collection = (Collection<Object>) currentMap.get(currentPath);
                        }

                        var toAdd = false;

                        if (!currentIds.get(ReflectionUtils.getType(field)).getRight().containsKey(path[i + 1])) {
                            currentMap = currentIds.get(ReflectionUtils.getType(field)).getRight();
                            toAdd = currentMap.isEmpty();

                        } else {
                            currentMap = currentIds.get(ReflectionUtils.getType(field)).getRight();
                        }

                        if (newCollection || toAdd)
                            collection.add(currentMap);

                    } else {
                        Map<String, Object> tempCurrentMap;
                        if (currentIds.containsKey(ReflectionUtils.getType(field))) {
                            tempCurrentMap = currentIds.get(ReflectionUtils.getType(field)).getRight();

                        } else {
                            tempCurrentMap = new LinkedHashMap<>();
                        }

                        var nestedMap = (Map<String, Object>) currentMap.computeIfAbsent(currentPath, k -> tempCurrentMap);
                        currentMap = nestedMap;
                    }

                }

                currentClass = ReflectionUtils.getType(field);
            }
        }
    }


    public static List<Map<String, Object>> toMap(List<Tuple> tuple, Class<?> entityClass, List<Selection<?>> selections, Map<Class<?>, Map<String, Field>> idFields) {
        Map<ClassID, Map<String, Object>> map = new LinkedHashMap<>();
        tuple.forEach(t -> toMap(t, map, entityClass, selections, idFields));
        return map.entrySet().stream().filter(e -> e.getKey().getClazz().equals(entityClass)).map(Map.Entry::getValue).toList();
    }

    @Data
    @AllArgsConstructor
    private static class ClassID {
        private Class<?> clazz;
        private JPAEntityId id;
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
