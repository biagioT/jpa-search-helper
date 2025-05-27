package app.tozzi.util;

import app.tozzi.exception.JPASearchException;
import app.tozzi.model.JPASearchOperatorFilter;
import app.tozzi.model.JPASearchOperatorGroup;
import app.tozzi.model.JPASearchPaginationFilter;
import app.tozzi.model.JPASearchSortType;
import app.tozzi.model.input.JPASearchInput;
import jakarta.persistence.criteria.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;
import java.util.TreeMap;

public class JPASearchUtils {

    public static JPASearchInput EMPTY_INPUT = new JPASearchInput();

    private static final String SEPARATOR = ",";
    private static final String ESCAPE_SEPARATOR_CHAR = "/";
    private static final String IGNORE_CASE_OPTION_IDENTIFIER = "#i";
    private static final String NEGATION_OPTION_IDENTIFIER = "#n";
    private static final String TRIM_OPTION_IDENTIFIER = "#t";
    private static final String PROJECTION_KEY = "selections";

    public static JPASearchInput toObject(Map<String, String> filters,
                                          boolean processPaginationOptions, boolean processSortOption, boolean processProjection
    ) {

        if (filters == null || filters.isEmpty()) {

            if (processSortOption) {
                throw new JPASearchException("Invalid sort filters");
            }

            if (processPaginationOptions) {
                throw new JPASearchException("Invalid pagination filters");
            }

            if (processProjection) {
                throw new JPASearchException("Invalid projection selection");
            }

            return JPASearchUtils.EMPTY_INPUT;
        }

        var res = new JPASearchInput();
        res.setFilter(new JPASearchInput.RootFilter());
        res.getFilter().setOperator(JPASearchOperatorGroup.AND.getValue());

        filters.entrySet().stream()
                .filter(e -> e.getValue() != null && e.getKey() != null && !e.getKey().isBlank())
                .forEach(e -> {
                    if ((processPaginationOptions || processSortOption) && (e.getKey().charAt(0) == '_' && JPASearchPaginationFilter.keys().contains(e.getKey().substring(1)) || e.getKey().endsWith("_" + JPASearchPaginationFilter.SORT.getValue()))) {
                        if (res.getOptions() == null) {
                            res.setOptions(new JPASearchInput.JPASearchOptions());
                        }

                        var paginationFilter = e.getKey().endsWith("_" + JPASearchPaginationFilter.SORT.getValue()) ? JPASearchPaginationFilter.SORT : JPASearchPaginationFilter.load(e.getKey().substring(1));
                        switch (paginationFilter) {
                            case LIMIT -> res.getOptions().setPageSize(GenericUtils.loadInt(e.getValue(), processPaginationOptions ? 0 : -1));
                            case OFFSET -> res.getOptions().setPageOffset(GenericUtils.loadInt(e.getValue(), 0));
                            case SORT -> {
                                if (res.getOptions().getSortOptions() == null) {
                                    res.getOptions().setSortOptions(new ArrayList<>());
                                }

                                var sortOption = new JPASearchInput.JPASortOptions();
                                sortOption.setKey(e.getKey().substring(0, e.getKey().lastIndexOf("_")));
                                sortOption.setDesc(JPASearchSortType.DESC.name().equalsIgnoreCase(e.getValue()));
                                res.getOptions().getSortOptions().add(sortOption);
                            }
                        }

                        return;
                    }

                    if (processProjection && e.getKey().equals(PROJECTION_KEY)) {
                        if (res.getOptions() == null) {
                            res.setOptions(new JPASearchInput.JPASearchOptions());
                        }

                        res.getOptions().setSelections(GenericUtils.split(e.getValue(), ",", null));
                        return;
                    }

                    var field = e.getKey().contains("_") ? e.getKey().substring(0, e.getKey().lastIndexOf("_")) : e.getKey();
                    var operator = e.getKey().contains("_") ? e.getKey().substring(e.getKey().lastIndexOf("_") + 1) : JPASearchOperatorFilter.EQ.getValue();
                    var ignoreCase = false;
                    if (operator.contains(IGNORE_CASE_OPTION_IDENTIFIER)) {
                        ignoreCase = true;
                        operator = operator.replace(IGNORE_CASE_OPTION_IDENTIFIER, "");
                    }
                    var negation = false;
                    if (operator.contains(NEGATION_OPTION_IDENTIFIER)) {
                        negation = true;
                        operator = operator.replace(NEGATION_OPTION_IDENTIFIER, "");
                    }

                    var trim = false;
                    if (operator.contains(TRIM_OPTION_IDENTIFIER)) {
                        trim = true;
                        operator = operator.replace(TRIM_OPTION_IDENTIFIER, "");
                    }

                    JPASearchInput.FieldFilter filter;

                    if (GenericUtils.containsSeparator(e.getValue(), SEPARATOR, ESCAPE_SEPARATOR_CHAR)) {
                        filter = new JPASearchInput.FilterMultipleValues();
                        ((JPASearchInput.FilterMultipleValues) filter).setValues(new ArrayList<>(GenericUtils.split(e.getValue(), SEPARATOR, ESCAPE_SEPARATOR_CHAR)));

                    } else if (GenericUtils.containsSeparatorWithEscape(e.getValue(), SEPARATOR, ESCAPE_SEPARATOR_CHAR)) {
                        filter = new JPASearchInput.FilterSingleValue();
                        ((JPASearchInput.FilterSingleValue) filter).setValue(e.getValue().replace(ESCAPE_SEPARATOR_CHAR, ""));

                    } else {
                        filter = new JPASearchInput.FilterSingleValue();
                        ((JPASearchInput.FilterSingleValue) filter).setValue(e.getValue());
                    }

                    filter.setKey(field);
                    filter.setOperator(operator);
                    if (ignoreCase || negation || trim) {
                        filter.setOptions(new JPASearchInput.JPASearchFilterOptions());
                        filter.getOptions().setIgnoreCase(ignoreCase);
                        filter.getOptions().setNegate(negation);
                        filter.getOptions().setTrim(trim);
                    }

                    if (res.getFilter().getFilters() == null) {
                        res.getFilter().setFilters(new ArrayList<>());
                    }

                    res.getFilter().getFilters().add(filter);

                });

        return res;
    }

    public static Predicate[] toPredicates(Expression<Boolean>[] values) {
        var predicates = new Predicate[values.length];
        for (var i = 0; i < values.length; i++) {
            predicates[i] = (Predicate) values[i];
        }
        return predicates;
    }

    public static <E> Root<E> fetchManagement(Map<String, JoinType> fetchMap, Root<E> root) {

        if (fetchMap != null) {
            fetchMap = new TreeMap<>(fetchMap);
            var doneFetches = new ArrayList<>();

            fetchMap.forEach((k, v) -> {
                if (k.contains(".")) {
                    var it = Arrays.stream(k.split("\\.")).iterator();
                    Fetch<?, ?> fetch;
                    var f = it.next();
                    var tempPath = new StringBuilder(f);

                    if (!doneFetches.contains(f)) {
                        fetch = root.fetch(f, v);
                        doneFetches.add(f);

                    } else {
                        fetch = root.getFetches().stream().filter(rf -> rf.getAttribute().getName().equals(f)).findAny().orElseThrow();
                    }

                    while (it.hasNext()) {
                        var f1 = it.next();
                        tempPath.append(".").append(f1);

                        if (!doneFetches.contains(tempPath.toString())) {
                            fetch = fetch.fetch(f1, v);
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

        return root;
    }

    public static <T> Expression<T> getPath(Root<?> root, String k) {

        if (k.contains(".")) {

            Path<T> path = null;

            for (var f : k.split("\\.")) {
                path = path == null ? root.get(f) : path.get(f);
            }

            return path;

        } else {
            return root.get(k);
        }
    }

}
