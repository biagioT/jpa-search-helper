package app.tozzi;

import app.tozzi.annotations.Searchable;
import app.tozzi.exceptions.InvalidFieldException;
import app.tozzi.exceptions.InvalidValueException;
import app.tozzi.model.*;
import app.tozzi.utils.GenericUtils;
import app.tozzi.utils.ReflectionUtils;
import jakarta.persistence.criteria.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NonNull;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;

import java.beans.PropertyDescriptor;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

public class JPASearchCore {

    public static <R, T> Specification<R> specification(Map<String, String> filters,
                                                        Class<T> clazz,
                                                        boolean throwsIfNotExists,
                                                        boolean throwsIfNotSearchable) {

        return specification(filters, clazz, null, throwsIfNotExists, throwsIfNotSearchable, null);
    }

    public static <R, T> Specification<R> specification(Map<String, String> filters,
                                                        Class<T> clazz,
                                                        boolean throwsIfNotExists,
                                                        boolean throwsIfNotSearchable, Map<String, String> entityFieldMap) {

        return specification(filters, clazz, null, throwsIfNotExists, throwsIfNotSearchable, entityFieldMap);
    }

    public static <R, T> Specification<R> specification(Map<String, String> filters,
                                                        Class<T> clazz,
                                                        Map<String, JoinType> fetchMap,
                                                        boolean throwsIfNotExists,
                                                        boolean throwsIfNotSearchable) {

        return specification(filters, clazz, fetchMap, throwsIfNotExists, throwsIfNotSearchable, null);
    }

    public static PageRequest loadSortAndPagination(Map<String, String> filters, Class<?> clazz, boolean throwsIfNotSortable, boolean throwsIfNotExists, boolean throwsIfNotSearchable, Map<String, String> entityFieldMap) {


        final AtomicInteger limit = new AtomicInteger(-1);
        final AtomicInteger offset = new AtomicInteger(-1);
        final AtomicReference<Sort> sort = new AtomicReference<>();

        filters.entrySet().stream().filter(e -> PaginationFilter.keys().stream().anyMatch(p -> e.getKey().endsWith(p)))
                .forEach(e -> {

                    String suffix = e.getKey().contains("_") ? e.getKey().substring(e.getKey().lastIndexOf("_")) : e.getKey();
                    PaginationFilter pk = PaginationFilter.load(suffix);
                    if (pk != null) {
                        switch (pk) {
                            case LIMIT -> limit.set(GenericUtils.loadInt(e.getValue(), -1));
                            case OFFSET -> offset.set(GenericUtils.loadInt(e.getValue(), -1));
                            case SORT -> {
                                DescriptorBean descriptor = loadDescriptor(e.getKey(), clazz, throwsIfNotExists, throwsIfNotSearchable, true, throwsIfNotSortable, entityFieldMap);
                                if (descriptor != null) {
                                    Sort s = Sort.by(descriptor.entityKey);
                                    switch (SortType.load(e.getValue(), SortType.ASC)) {
                                        case ASC -> s = s.ascending();
                                        case DESC -> s = s.descending();
                                    }

                                    sort.set(s);
                                }
                            }
                            default -> throw new InvalidFieldException("Invalid key " + e.getKey(), e.getKey());
                        }
                    }
                });

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

    public static <R, T> Specification<R> specification(Map<String, String> filters,
                                                        Class<T> clazz,
                                                        Map<String, JoinType> fetchMap,
                                                        boolean throwsIfNotExists,
                                                        boolean throwsIfNotSearchable, Map<String, String> entityFieldMap) {

        return (root, query, criteriaBuilder) -> {
            fetchManagement(fetchMap, root);
            return criteriaBuilder.and(andPredicates(filters, root, criteriaBuilder, clazz, throwsIfNotExists, throwsIfNotSearchable, entityFieldMap).toArray(Predicate[]::new));
        };
    }

    private static void fetchManagement(Map<String, JoinType> fetchMap, Root<?> root) {

        if (fetchMap != null) {
            fetchMap.forEach((k, v) -> {
                if (k.contains(".")) {
                    Fetch<?, ?> fetch = null;
                    for (String f : k.split("\\.")) {
                        if (fetch == null) {
                            fetch = root.fetch(f, v);

                        } else {
                            fetch.fetch(f, v);
                        }
                    }

                } else {
                    root.fetch(k, v);
                }
            });
        }
    }

    private static <T> List<Predicate> andPredicates(@NonNull Map<String, String> filters, @NonNull Root<?> root, @NonNull CriteriaBuilder criteriaBuilder, @NonNull Class<T> clazz, boolean throwsIfNotExists, boolean throwsIfNotSearchable, Map<String, String> entityFieldMap) {
        return filters.entrySet().stream()
                .filter(e -> PaginationFilter.keys().stream().noneMatch(p -> e.getKey().endsWith(p)))
                .map(e -> filterManagement(e.getKey(), e.getValue(), clazz, throwsIfNotExists, throwsIfNotSearchable, entityFieldMap))
                .filter(Objects::nonNull)
                .map(f -> f.searchFilter.getFunction().apply(new FieldRootBuilderBean<>(f.fieldKey, root, criteriaBuilder, f.value, f.trim)))
                .toList();
    }

    public static <T> FilterBean filterManagement(String key, String value, Class<T> clazz, boolean throwsIfNotExists, boolean throwsIfNotSearchable, Map<String, String> entityFieldMap) {

        if (key == null || key.isBlank() || value == null || value.isBlank()) {
            return null;
        }

        DescriptorBean descriptor = loadDescriptor(key, clazz, throwsIfNotExists, throwsIfNotSearchable, false, false, entityFieldMap);

        if (descriptor == null) {
            return null;
        }

        SearchFilter searchFilter = SearchFilter.load(descriptor.suffix, value, SearchFilter.EQ);
        if (searchFilter.hasFixedValue()) {
            return new FilterBean(descriptor.entityKey, descriptor.path, searchFilter, value, descriptor.searchable.trim());
        }

        preValidations(descriptor.searchable, descriptor.path, value);
        SearchType searchType = SearchType.UNTYPED.equals(descriptor.searchable.targetType()) ? SearchType.load(descriptor.propertyDescriptor.getPropertyType(), SearchType.STRING) : descriptor.searchable.targetType();
        Object targetValue = searchType.getValue(descriptor.path, value, descriptor.searchable.datePattern(), descriptor.searchable.decimalFormat());
        searchableValidations(targetValue, descriptor.searchable, descriptor.path, value, searchType);
        filterValidations(searchFilter, descriptor.path, targetValue, searchType);
        return new FilterBean(descriptor.entityKey, descriptor.path, searchFilter, targetValue, descriptor.searchable.trim());
    }

    private static DescriptorBean loadDescriptor(String key, Class<?> clazz, boolean throwsIfNotExists, boolean throwsIfNotSearchable, boolean checkSortable, boolean throwsIfNotSortable, Map<String, String> entityFieldMap) {
        String fullField = key.contains("_") ? key.substring(0, key.lastIndexOf("_")) : key;
        String suffixFilter = key.contains("_") ? key.substring(key.lastIndexOf("_")) : key;

        PropertyDescriptor pd = null;

        String field = fullField;
        if (key.contains(".")) {
            String[] paths = field.split("\\.");
            int count = 0;
            Class<?> cl = clazz;

            while (count < paths.length) {
                field = paths[count];
                pd = ReflectionUtils.getPropertyDescriptor(field, cl);

                if (pd == null) {
                    break;
                }

                cl = pd.getPropertyType();
                count++;
            }

        } else {
            pd = ReflectionUtils.getPropertyDescriptor(field, clazz);
        }

        if (pd == null) {
            if (throwsIfNotExists) {
                throw new InvalidFieldException("Field [" + fullField + "] does not exists", fullField);
            }

            return null;
        }

        if (!ReflectionUtils.hasAnnotation(field, pd, Searchable.class)) {
            if (throwsIfNotSearchable) {
                throw new InvalidFieldException("Field [" + fullField + "] is not searchable", fullField);
            }

            return null;
        }

        Searchable searchable = ReflectionUtils.getAnnotation(field, pd, Searchable.class);

        if (checkSortable && !searchable.sortable()) {
            if (throwsIfNotSortable) {
                throw new InvalidFieldException("Field [" + fullField + "] is not sortable", fullField);
            }

            return null;
        }

        return new DescriptorBean(fullField, suffixFilter, searchable, pd, entityFieldMap != null ? entityFieldMap.get(field) : (searchable.entityFieldKey().isBlank() ? field : searchable.entityFieldKey()));
    }

    private static void filterValidations(SearchFilter searchFilter, String field, Object valueObj, SearchType searchType) {
        boolean isCollection = Collection.class.isAssignableFrom(valueObj.getClass());
        Collection<?> collection = isCollection ? (Collection<?>) valueObj : null;
        int values = isCollection ? collection.size() : 1;

        if (searchFilter.getAllowedValues() != -1 && searchFilter.getAllowedValues() != values) {
            throw new InvalidValueException("Invalid values count: [" + values + "] for type [" + searchType.name() + "] of field [" + field + "]. Expected: [" + searchFilter.getAllowedValues() + "]; received: [" + values + "]", field, valueObj);
        }

        boolean isComparable = isCollection ? collection.stream().anyMatch(v -> Comparable.class.isAssignableFrom(v.getClass())) : Comparable.class.isAssignableFrom(valueObj.getClass());
        if (!isComparable && searchFilter.isComparable()) {
            throw new InvalidFieldException("Not allowed filter [" + searchFilter.getSuffix() + "] for type [" + searchType.name() + "] of field [" + field + "]", field);
        }
    }

    private static void preValidations(Searchable searchable, String field, String value) {
        // Regex
        if (searchable.regexPattern() != null && !searchable.regexPattern().isBlank() && !value.matches(searchable.regexPattern())) {
            throw new InvalidValueException("Value [" + value + " does not match pattern [" + searchable.regexPattern() + " of field [" + field + "]", field, value);
        }
    }

    private static void searchableValidations(Object targetValue, Searchable searchable, String field, String value, SearchType searchType) {

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
    }

    @Data
    @AllArgsConstructor
    public static class FilterBean {
        private String fieldKey;
        private String originalKey;
        private SearchFilter searchFilter;
        private Object value;
        private boolean trim;
    }

    @Data
    @AllArgsConstructor
    public static class DescriptorBean {
        private String path;
        private String suffix;
        private Searchable searchable;
        private PropertyDescriptor propertyDescriptor;
        private String entityKey;
    }
}
