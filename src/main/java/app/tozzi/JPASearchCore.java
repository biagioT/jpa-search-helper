package app.tozzi;

import app.tozzi.annotations.Searchable;
import app.tozzi.annotations.Tag;
import app.tozzi.exceptions.InvalidFieldException;
import app.tozzi.exceptions.InvalidValueException;
import app.tozzi.model.*;
import app.tozzi.exceptions.JPASearchException;
import app.tozzi.utils.ReflectionUtils;
import javax.persistence.criteria.*;

import com.fasterxml.jackson.databind.JsonNode;
import lombok.AllArgsConstructor;
import lombok.Data;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;

import static app.tozzi.JPASearchFunctions.getPath;

public class JPASearchCore {
    public static <R, T> Specification<R> specification(JsonNode filterPayload,
                                                        Class<T> entityClass,
                                                        boolean throwsIfNotExistsOrNotSearchable) {
        return specification(filterPayload, entityClass, null, throwsIfNotExistsOrNotSearchable, null);
    }

    public static <R, T> Specification<R> specification(JsonNode filterPayload,
                                                        Class<T> entityClass,
                                                        Map<String, JoinType> fetchMap,
                                                        boolean throwsIfNotExistsOrNotSearchable) {
        return specification(filterPayload, entityClass, fetchMap, throwsIfNotExistsOrNotSearchable, null);
    }

    public static <R, T> Specification<R> specification(JsonNode filterPayload,
                                                        Class<T> entityClass,
                                                        boolean throwsIfNotExistsOrNotSearchable,
                                                        Map<String, String> entityFieldMap) {
        return specification(filterPayload, entityClass, null, throwsIfNotExistsOrNotSearchable, entityFieldMap);
    }

    public static <R, T> Specification<R> specification(JsonNode filterPayload,
                                                        Class<T> entityClass,
                                                        Map<String, JoinType> fetchMap,
                                                        boolean throwsIfNotExistsOrNotSearchable,
                                                        Map<String, String> entityFieldMap) {


        var filterExpression = filterPayload.get("filter");
        return (root, query, criteriaBuilder) -> {
            fetchManagement(fetchMap, root);
            var expr = processExpression(filterExpression, criteriaBuilder, root, entityClass, throwsIfNotExistsOrNotSearchable, entityFieldMap);
            if (expr instanceof Predicate) {
                return (Predicate) expr;
            } else {
                throw new JPASearchException("Not resulting a predicate" + expr);
            }
        };
    }

    private static <T> Expression<?> processValue(
        JsonNode node,
        CriteriaBuilder cb,
        Root<?> root,
        Class<T> entityClass,
        boolean throwsIfNotExistsOrNotSearchable,
        Map<String, String> entityFieldMap
    ) {
        if (node.isTextual()) {
            // textual means field name if it starts with :.
            // ["eq", ":field", "asdf"]
            var asText = node.asText();
            if (asText.startsWith(":")) {
                var fieldName = asText.substring(1);
                var descriptor = loadDescriptor(
                    fieldName,
                    throwsIfNotExistsOrNotSearchable,
                    false,
                    false,
                    entityFieldMap,
                    ReflectionUtils.getAllSearchableFields(entityClass)
                );
                var path = getPath(root, fieldName); //.as(descriptor.searchType.getDefaultClasses().stream().findFirst().orElseThrow());
                if (descriptor.searchable.trim() && descriptor.searchType == SearchType.STRING) {
                    return cb.trim(path.as(String.class));
                } else {
                    return path;
                }
            } else {
                return cb.literal(asText);
            }
        } else if (node.isInt()) {
            return cb.literal(node.asInt());
        } else if (node.isLong()) {
            return cb.literal(node.asLong());
        } else if (node.isDouble()) {
            return cb.literal(node.asDouble());
        } else if (node.isBoolean()) {
            return cb.literal(node.asBoolean());
        } else if (node.isArray()) {
            return processExpression(node, cb, root, entityClass, throwsIfNotExistsOrNotSearchable, entityFieldMap);
        } else if (node.isNull()) {
            return cb.nullLiteral(entityClass); // entityClass? //literal(null);
        } else {
            throw new JPASearchException("unexpected: " + node);
        }
    }

    private static <T> Expression<T> processExpression(
        JsonNode node,
        CriteriaBuilder cb,
        Root<?> root,
        Class<T> entityClass,
        boolean throwsIfNotExistsOrNotSearchable,
        Map<String, String> entityFieldMap
    ) {
        if (!node.isArray() || node.isEmpty() || !node.get(0).isTextual()) {
            throw new JPASearchException("Invalid expression");
        }

        var operator = Operator.load(node.get(0).textValue());
        var arguments = new ArrayList<Expression<?>>();

        for (var i = 1; i< node.size(); i++) {
            var child = node.get(i);
            arguments.add(
                processValue(
                    child,
                    cb,
                    root,
                    entityClass,
                    throwsIfNotExistsOrNotSearchable,
                    entityFieldMap
                )
            );
        }

        return operator.getFunction().apply(cb, arguments.toArray(new Expression[0]));
    }


    private static void fetchManagement(Map<String, JoinType> fetchMap, Root<?> root) {

        if (fetchMap != null) {
            List<String> doneFetches = new ArrayList<>();

            fetchMap.forEach((k, v) -> {
                if (k.contains(".")) {
                    Iterator<String> it = Arrays.stream(k.split("\\.")).iterator();
                    Fetch<?, ?> fetch;
                    String f = it.next();
                    StringBuilder tempPath = new StringBuilder(f);

                    if (!doneFetches.contains(f)) {
                        fetch = root.fetch(f, v);
                        doneFetches.add(f);

                    } else {
                        fetch = root.getFetches().stream().filter(rf -> rf.getAttribute().getName().equals(f)).findAny().orElseThrow();
                    }

                    while (it.hasNext()) {
                        String f1 = it.next();
                        tempPath.append(".").append(f1);

                        if (!doneFetches.contains(tempPath.toString())) {
                            fetch = fetch.fetch(it.next(), v);
                            doneFetches.add(tempPath.toString());

                        } else {
                            fetch = fetch.getFetches().stream().filter(rf -> rf.getAttribute().getName().equals(f1)).findAny().orElseThrow();
                        }
                    }

                } else if (!doneFetches.contains(k)) {
                    root.fetch(k, v);
                    doneFetches.add(k);
                }
            });
        }
    }

    public static Sort loadSort(JsonNode filters, Class<?> clazz, boolean throwsIfNotSortable, boolean throwsIfNotExistsOrNotSearchable, Map<String, String> entityFieldMap) {
        return null;
        //return filters.entrySet().stream().filter(e -> e.getKey().endsWith(PaginationFilter.SORT.getSuffix()))
        //        .map(e -> new AbstractMap.SimpleEntry<>(e.getValue(),
        //                loadDescriptor(e.getKey(), throwsIfNotExistsOrNotSearchable, true, throwsIfNotSortable, entityFieldMap,
        //                        ReflectionUtils.getAllSearchableFields(clazz))))
        //        .filter(e -> e.getValue() != null)
        //        .map(e -> {
        //            Sort s = Sort.by(e.getValue().getEntityKey());
        //            switch (SortType.load(e.getKey(), SortType.ASC)) {
        //                case ASC -> s = s.ascending();
        //                case DESC -> s = s.descending();
        //            }
        //            return s;

        //        }).findAny().orElseThrow(() -> new InvalidFieldException("Invalid or not present sort key", PaginationFilter.SORT.getSuffix()));

    }

    public static PageRequest loadSortAndPagination(JsonNode filters, Class<?> clazz, boolean throwsIfNotSortable, boolean throwsIfNotExistsOrSearchable, Map<String, String> entityFieldMap) {

        final AtomicInteger limit = new AtomicInteger(-1);
        final AtomicInteger offset = new AtomicInteger(-1);
        final AtomicReference<Sort> sort = new AtomicReference<>();

        //filters.entrySet().stream().filter(e -> PaginationFilter.keys().stream().anyMatch(p -> e.getKey().endsWith(p)))
        //        .forEach(e -> {

        //            String suffix = e.getKey().contains("_") ? e.getKey().substring(e.getKey().lastIndexOf("_")) : e.getKey();
        //            PaginationFilter pk = PaginationFilter.load(suffix);
        //            if (pk != null) {
        //                switch (pk) {
        //                    case LIMIT -> limit.set(GenericUtils.loadInt(e.getValue(), -1));
        //                    case OFFSET -> offset.set(GenericUtils.loadInt(e.getValue(), -1));
        //                    case SORT -> {
        //                        DescriptorBean descriptor = loadDescriptor(e.getKey(), throwsIfNotExistsOrSearchable, true, throwsIfNotSortable, entityFieldMap,
        //                                ReflectionUtils.getAllSearchableFields(clazz));
        //                        if (descriptor != null) {
        //                            Sort s = Sort.by(descriptor.entityKey);
        //                            switch (SortType.load(e.getValue(), SortType.ASC)) {
        //                                case ASC -> s = s.ascending();
        //                                case DESC -> s = s.descending();
        //                            }

        //                            sort.set(s);
        //                        }
        //                    }
        //                    default -> throw new InvalidFieldException("Invalid key " + e.getKey(), e.getKey());
        //                }
        //            }
        //        });

        if (limit.get() == -1) {
            throw new InvalidFieldException("Invalid or not present limit", PaginationFilter.LIMIT.getSuffix());
        }

        PageRequest result = PageRequest.ofSize(limit.get());
        if (offset.get() != -1) {
            result = result.withPage(offset.get());
        }
        if (sort.get() != null) {
            result = result.withSort(sort.get());
        }

        return result;
    }

    //private static <T> List<Predicate> andPredicates(@NonNull Map<String, String> filters, @NonNull Root<?> root, @NonNull CriteriaBuilder criteriaBuilder, @NonNull Class<T> clazz,
    //                                                 boolean throwsIfNotExistsOrSearchable, Map<String, String> entityFieldMap) {
    //    return filters.entrySet().stream()
    //            .filter(e -> PaginationFilter.keys().stream().noneMatch(p -> e.getKey().endsWith(p)))
    //            .map(e -> filterManagement("OP!!!!", e.getKey(), e.getValue(), clazz, throwsIfNotExistsOrSearchable, entityFieldMap))
    //            .filter(Objects::nonNull)
    //            .map(f -> f.operator.getFunction().apply(new FieldRootBuilderBean<>(f.fieldKey, root, criteriaBuilder, f.value, f.trim)))
    //            .toList();
    //}

    public static <T> FilterBean filterManagement(String operatorStr, String key, String value, Class<T> clazz, boolean throwsIfNotExistsOrSearchable, Map<String, String> entityFieldMap) {

        if (key == null || key.isBlank() || value == null) {
            return null;
        }

        DescriptorBean descriptor = loadDescriptor(key, throwsIfNotExistsOrSearchable, false, false, entityFieldMap, ReflectionUtils.getAllSearchableFields(clazz));

        if (descriptor == null) {
            return null;
        }

        Operator operator = Operator.load(operatorStr);

        searchableValidations(descriptor.searchable, descriptor.path, operator);
        Object targetValue = descriptor.searchType.getValue(descriptor.path, value, descriptor.searchable.datePattern(), descriptor.searchable.decimalFormat(), operator.isNoNumberParsing());
        searchableValidationsOnTargetValue(targetValue, descriptor.searchable, descriptor.path, value, descriptor.searchType);
        filterValidations(operator, descriptor.path, targetValue, descriptor.searchType);
        return new FilterBean(descriptor.entityKey, descriptor.path, operator, targetValue, descriptor.searchable.trim());
    }

    private static DescriptorBean loadDescriptor(String key, boolean throwsIfNotExistsOrNotSortable, boolean checkSortable,
                                                 boolean throwsIfNotSortable, Map<String, String> entityFieldMap, Map<String, Pair<Searchable, Class<?>>> searchableFields) {

        String fullField = key.contains("_") ? key.substring(0, key.lastIndexOf("_")) : key;

        Map<String, Pair<Pair<Searchable, Class<?>>, Tag>> tagMap = new HashMap<>();
        searchableFields.entrySet().stream().filter(e -> e.getValue().getKey().tags() != null && e.getValue().getKey().tags().length > 0)
                .forEach(e -> Stream.of(e.getValue().getKey().tags()).forEach(t -> {
                    tagMap.put(t.fieldKey(), Pair.of(e.getValue(), t));
                }));

        if (!searchableFields.containsKey(fullField) && !tagMap.containsKey(fullField)) {

            if (throwsIfNotExistsOrNotSortable) {
                throw new InvalidFieldException("Field [" + fullField + "] does not exists or not sortable", fullField);
            }

            return null;
        }

        Searchable searchable = searchableFields.containsKey(fullField) ? searchableFields.get(fullField).getKey() : tagMap.get(fullField).getKey().getKey();
        Class<?> type = searchableFields.containsKey(fullField) ? searchableFields.get(fullField).getValue() : tagMap.get(fullField).getKey().getValue();

        if (checkSortable && !searchable.sortable()) {
            if (throwsIfNotSortable) {
                throw new InvalidFieldException("Field [" + fullField + "] is not sortable", fullField);
            }

            return null;
        }

        String entityField = entityFieldMap != null && entityFieldMap.containsKey(fullField) ? entityFieldMap.get(fullField) :
                (tagMap.containsKey(fullField) ?
                        (tagMap.get(fullField).getRight().entityFieldKey() != null && !tagMap.get(fullField).getRight().entityFieldKey().isBlank() ? tagMap.get(fullField).getRight().entityFieldKey() : fullField)
                        : (searchable.entityFieldKey() != null && !searchable.entityFieldKey().isBlank() ? searchable.entityFieldKey() : fullField));

        return new DescriptorBean(fullField, searchable,
                SearchType.UNTYPED.equals(searchable.targetType()) ? SearchType.load(type, SearchType.STRING) : searchable.targetType(), entityField);
    }

    private static void filterValidations(Operator operator, String field, Object valueObj, SearchType searchType) {
        boolean isCollection = Collection.class.isAssignableFrom(valueObj.getClass());
        Collection<?> collection = isCollection ? (Collection<?>) valueObj : null;
        int values = isCollection ? collection.size() : 1;

        if (operator.getAllowedValues() != -1 && operator.getAllowedValues() != values) {
            throw new InvalidValueException("Invalid values count: [" + values + "] for type [" + searchType.name() + "] of field [" + field + "]. Expected: [" + operator.getAllowedValues() + "]; received: [" + values + "]", field, valueObj);
        }

        boolean isComparable = isCollection ? collection.stream().anyMatch(v -> Comparable.class.isAssignableFrom(v.getClass())) : Comparable.class.isAssignableFrom(valueObj.getClass());
        if (!isComparable && operator.isComparable()) {
            throw new InvalidFieldException("Not allowed filter [" + operator.getName() + "] for type [" + searchType.name() + "] of field [" + field + "]", field);
        }
    }

    private static void searchableValidations(Searchable searchable, String field, Operator operator) {

        if (searchable.allowedFilters() != null && searchable.allowedFilters().length > 0 && Stream.of(searchable.allowedFilters()).noneMatch(sf -> sf.equals(operator))) {
            throw new InvalidFieldException("Not allowed filters [" + operator.getName() + "] for field [" + field + "]", field);
        }

        if (searchable.notAllowedFilters() != null && searchable.notAllowedFilters().length > 0 && Stream.of(searchable.notAllowedFilters()).anyMatch(sf -> sf.equals(operator))) {
            throw new InvalidFieldException("Not allowed filters [" + operator.getName() + "] for field [" + field + "]", field);
        }

        if (!searchable.likeFilters() && operator.isLike()) {
            throw new InvalidFieldException("Not allowed filters [" + operator.getName() + "] for field [" + field + "]", field);
        }

    }

    private static void searchableValidationsOnTargetValue(Object targetValue, Searchable searchable, String field, String value, SearchType searchType) {

        // Length
        int maxLength = searchType.getMaxLength(targetValue);
        if (maxLength >= 0 && searchable.maxSize() >= 0 && maxLength > searchable.maxSize()) {
            throw new InvalidValueException("Value [" + value + "] exceeds maximum length [" + searchable.maxSize() + "] defined on field [" + field + "]", field, value);
        }
        int minLength = searchType.getMinLength(targetValue);
        if (minLength >= 0 && searchable.minSize() >= 0 && minLength < searchable.minSize()) {
            throw new InvalidValueException("Value [" + value + "] less than minimum length [" + searchable.minSize() + "] defined on field [" + field + "]", field, value);
        }

        // Digits
        int maxDigits = searchType.getMaxDigits(targetValue);
        if (maxDigits >= 0 && searchable.maxDigits() >= 0 && maxDigits > searchable.maxDigits()) {
            throw new InvalidValueException("Value [" + value + "] exceeds maximum digits count [" + searchable.maxDigits() + "] defined on field [" + field + "]", field, value);
        }
        int minDigits = searchType.getMinDigits(targetValue);
        if (minLength >= 0 && searchable.minDigits() >= 0 && minDigits < searchable.minDigits()) {
            throw new InvalidValueException("Value [" + value + "] less than minimum digits count [" + searchable.minDigits() + "] defined on field [" + field + "]", field, value);
        }

        // Regex
        if (searchable.regexPattern() != null && !searchable.regexPattern().isBlank() && !searchType.matchRegex(targetValue, searchable.regexPattern())) {
            throw new InvalidValueException("Value [" + value + " does not match pattern [" + searchable.regexPattern() + " of field [" + field + "]", field, value);
        }
    }

    @Data
    @AllArgsConstructor
    public static class FilterBean {
        private String fieldKey;
        private String originalKey;
        private Operator operator;
        private Object value;
        private boolean trim;
    }

    @Data
    @AllArgsConstructor
    public static class DescriptorBean {
        private String path;
        private Searchable searchable;
        private SearchType searchType;
        private String entityKey;
    }
}
