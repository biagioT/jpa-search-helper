package app.tozzi.util;

import app.tozzi.exception.JPASearchException;
import app.tozzi.model.JPASearchOperatorFilter;
import app.tozzi.model.JPASearchOperatorGroup;
import app.tozzi.model.JPASearchPaginationFilter;
import app.tozzi.model.JPASearchSortType;
import app.tozzi.model.input.JPASearchInput;
import jakarta.persistence.criteria.*;

import java.util.*;

public class JPASearchUtils {

    public static final JPASearchInput EMPTY_INPUT = new JPASearchInput();

    private static final String SEPARATOR = ",";
    private static final String ESCAPE_SEPARATOR_CHAR = "/";
    private static final String IGNORE_CASE_OPTION_IDENTIFIER = "#i";
    private static final String NEGATION_OPTION_IDENTIFIER = "#n";
    private static final String TRIM_OPTION_IDENTIFIER = "#t";
    private static final String PROJECTION_KEY = "selections";

    private static final List<String> SORTED_OPERATORS = Arrays.stream(JPASearchOperatorFilter.values())
            .map(JPASearchOperatorFilter::getValue)
            .sorted((a, b) -> Integer.compare(b.length(), a.length()))
            .toList();

    public static JPASearchInput toObject(Map<String, String> filters, boolean processPagination, boolean processSort, boolean processProjection) {
        if (filters == null || filters.isEmpty()) {
            validateEmptyFilters(processPagination, processSort, processProjection);
            return EMPTY_INPUT;
        }

        var res = new JPASearchInput();
        res.setFilter(new JPASearchInput.RootFilter());
        res.getFilter().setOperator(JPASearchOperatorGroup.AND.getValue());
        res.setOptions(new JPASearchInput.JPASearchOptions());

        filters.forEach((key, value) -> {
            if (key == null || key.isBlank() || value == null) return;

            if ((processPagination || processSort) && isPaginationOrSortKey(key)) {
                handlePaginationAndSort(res, key, value, processPagination);
                return;
            }

            if (processProjection && PROJECTION_KEY.equals(key)) {
                res.getOptions().setSelections(GenericUtils.split(value, ",", null));
                return;
            }

            addFilter(res, key, value);
        });

        if (res.getOptions().getPageSize() == null && res.getOptions().getSortOptions() == null && res.getOptions().getSelections() == null) {
            res.setOptions(null);
        }

        return res;
    }

    private static void validateEmptyFilters(boolean pag, boolean sort, boolean proj) {
        if (sort) throw new JPASearchException("Invalid sort filters");
        if (pag) throw new JPASearchException("Invalid pagination filters");
        if (proj) throw new JPASearchException("Invalid projection selection");
    }

    private static boolean isPaginationOrSortKey(String key) {
        return (key.startsWith("_") && JPASearchPaginationFilter.keys().contains(key.substring(1))) ||
                key.endsWith("_" + JPASearchPaginationFilter.SORT.getValue());
    }

    private static void handlePaginationAndSort(JPASearchInput input, String key, String value, boolean processPagination) {
        var cleanKey = key.startsWith("_") ? key.substring(1) : key;

        if (key.endsWith("_" + JPASearchPaginationFilter.SORT.getValue())) {
            var fieldName = key.substring(0, key.lastIndexOf("_"));
            addSortOption(input, fieldName, value);
            return;
        }

        var paginationFilter = JPASearchPaginationFilter.load(cleanKey);
        switch (paginationFilter) {
            case LIMIT ->
                    input.getOptions().setPageSize(GenericUtils.loadInt(value, processPagination ? 0 : -1));
            case OFFSET -> input.getOptions().setPageOffset(GenericUtils.loadInt(value, 0));
            case SORT -> {
            }
        }
    }

    private static void addSortOption(JPASearchInput input, String field, String direction) {
        if (input.getOptions().getSortOptions() == null) {
            input.getOptions().setSortOptions(new ArrayList<>());
        }
        var sortOption = new JPASearchInput.JPASortOptions();
        sortOption.setKey(field);
        sortOption.setDesc(JPASearchSortType.DESC.name().equalsIgnoreCase(direction));
        input.getOptions().getSortOptions().add(sortOption);
    }

    private static void addFilter(JPASearchInput input, String rawKey, String value) {
        var tempKey = rawKey;
        var ignoreCase = false;
        var negation = false;
        var trim = false;
        var foundOption = false;

        do {
            foundOption = false;
            if (tempKey.endsWith(IGNORE_CASE_OPTION_IDENTIFIER)) {
                ignoreCase = true;
                tempKey = tempKey.substring(0, tempKey.length() - IGNORE_CASE_OPTION_IDENTIFIER.length());
                foundOption = true;
            }
            if (tempKey.endsWith(NEGATION_OPTION_IDENTIFIER)) {
                negation = true;
                tempKey = tempKey.substring(0, tempKey.length() - NEGATION_OPTION_IDENTIFIER.length());
                foundOption = true;
            }
            if (tempKey.endsWith(TRIM_OPTION_IDENTIFIER)) {
                trim = true;
                tempKey = tempKey.substring(0, tempKey.length() - TRIM_OPTION_IDENTIFIER.length());
                foundOption = true;
            }
        } while (foundOption);

        var field = tempKey;
        var operator = JPASearchOperatorFilter.EQ.getValue();

        for (var op : SORTED_OPERATORS) {
            var suffix = "_" + op;
            if (tempKey.endsWith(suffix)) {
                field = tempKey.substring(0, tempKey.length() - suffix.length());
                operator = op;
                break;
            }
        }

        JPASearchInput.FieldFilter filter;
        if (GenericUtils.containsSeparator(value, SEPARATOR, ESCAPE_SEPARATOR_CHAR)) {
            filter = new JPASearchInput.FilterMultipleValues();
            ((JPASearchInput.FilterMultipleValues) filter).setValues(new ArrayList<>(GenericUtils.split(value, SEPARATOR, ESCAPE_SEPARATOR_CHAR)));
        } else {
            filter = new JPASearchInput.FilterSingleValue();
            var cleanVal = GenericUtils.containsSeparatorWithEscape(value, SEPARATOR, ESCAPE_SEPARATOR_CHAR)
                    ? value.replace(ESCAPE_SEPARATOR_CHAR, "")
                    : value;
            ((JPASearchInput.FilterSingleValue) filter).setValue(cleanVal);
        }

        filter.setKey(field);
        filter.setOperator(operator);

        if (ignoreCase || negation || trim) {
            filter.setOptions(new JPASearchInput.JPASearchFilterOptions());
            filter.getOptions().setIgnoreCase(ignoreCase);
            filter.getOptions().setNegate(negation);
            filter.getOptions().setTrim(trim);
        }

        if (input.getFilter().getFilters() == null) {
            input.getFilter().setFilters(new ArrayList<>());
        }
        input.getFilter().getFilters().add(filter);
    }

    public static Predicate[] toPredicates(Expression<Boolean>[] values) {
        return Arrays.stream(values).map(v -> (Predicate) v).toArray(Predicate[]::new);
    }

    public static <E> Root<E> fetchManagement(Map<String, JoinType> fetchMap, Root<E> root) {
        if (fetchMap == null || fetchMap.isEmpty()) {
            return root;
        }

        var sortedMap = new TreeMap<>(fetchMap);
        var doneFetches = new HashSet<String>();

        sortedMap.forEach((path, type) -> {
            if (!path.contains(".")) {
                if (doneFetches.add(path)) {
                    root.fetch(path, type);
                }
            } else {
                Fetch<?, ?> currentFetch = null;
                var parts = path.split("\\.");
                var currentPath = new StringBuilder();

                for (int i = 0; i < parts.length; i++) {
                    var part = parts[i];
                    if (i > 0) {
                        currentPath.append(".");
                    }
                    currentPath.append(part);
                    var pathKey = currentPath.toString();

                    if (doneFetches.add(pathKey)) {
                        if (i == 0) {
                            currentFetch = root.fetch(part, type);
                        } else {
                            currentFetch = currentFetch.fetch(part, type);
                        }
                    } else {
                        if (i == 0) {
                            currentFetch = root.getFetches().stream()
                                    .filter(f -> f.getAttribute().getName().equals(part))
                                    .findAny().orElseThrow(() -> new JPASearchException("Fetch logic error for " + part));
                        } else {
                            var nextPart = part;
                            currentFetch = currentFetch.getFetches().stream()
                                    .filter(f -> f.getAttribute().getName().equals(nextPart))
                                    .findAny().orElseThrow(() -> new JPASearchException("Fetch logic error for " + nextPart));
                        }
                    }
                }
            }
        });

        return root;
    }

    public static <T> Expression<T> getPath(Root<?> root, String k) {
        if (!k.contains(".")) {
            return root.get(k);
        }

        Path<T> path = null;
        for (var f : k.split("\\.")) {
            path = path == null ? root.get(f) : path.get(f);
        }
        return path;
    }

    @SuppressWarnings("unchecked")
    public static <T> Expression<T> getPath(CriteriaBuilder cb, Root<?> root, String fieldName, String jsonPath) {
        Expression<?> expr = getPath(root, fieldName);

        if (jsonPath == null || jsonPath.isBlank()) {
            return (Expression<T>) expr;
        }

        var parts = jsonPath.split("\\.");
        for (int i = 0; i < parts.length; i++) {
            var isLast = (i == parts.length - 1);
            var functionName = isLast ? "jsonb_extract_path_text" : "jsonb_extract_path";
            var returnType = isLast ? String.class : Object.class;

            expr = cb.function(functionName, returnType, expr, cb.literal(parts[i]));
        }

        return (Expression<T>) expr;
    }
}