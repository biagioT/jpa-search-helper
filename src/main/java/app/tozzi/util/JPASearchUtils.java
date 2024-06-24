package app.tozzi.util;

import app.tozzi.model.JPASearchOperatorFilter;
import app.tozzi.model.JPASearchOperatorGroup;
import app.tozzi.model.JPASearchPaginationFilter;
import app.tozzi.model.input.JPASearchInput;
import jakarta.persistence.criteria.*;

import java.util.*;

public class JPASearchUtils {

    public static JPASearchInput toObject(Map<String, String> filters,
                                          boolean processPaginationOptions, boolean processSortOption,
                                          String separator, String escapeSeparatorChar, String ignoreCaseOptionIdentifier,
                                          String negationOptionIdentifier
    ) {

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
                                res.getOptions().setSortKey(e.getKey().substring(0, e.getKey().lastIndexOf("_")));
                                res.getOptions().setSortDesc(Boolean.parseBoolean(e.getValue()));
                            }
                        }

                        return;
                    }

                    var field = e.getKey().contains("_") ? e.getKey().substring(0, e.getKey().lastIndexOf("_")) : e.getKey();
                    var operator = e.getKey().contains("_") ? e.getKey().substring(e.getKey().lastIndexOf("_") + 1) : JPASearchOperatorFilter.EQ.getValue();
                    var ignoreCase = false;
                    if (operator.contains(ignoreCaseOptionIdentifier)) {
                        ignoreCase = true;
                        operator = operator.replace(ignoreCaseOptionIdentifier, "");
                    }
                    var negation = false;
                    if (operator.contains(negationOptionIdentifier)) {
                        negation = true;
                        operator = operator.replace(negationOptionIdentifier, "");
                    }

                    JPASearchInput.FieldFilter filter;

                    if (GenericUtils.containsSeparator(e.getValue(), separator, escapeSeparatorChar)) {
                        filter = new JPASearchInput.FilterMultipleValues();
                        ((JPASearchInput.FilterMultipleValues) filter).setValues(new ArrayList<>(GenericUtils.split(e.getValue(), separator, escapeSeparatorChar)));

                    } else {
                        filter = new JPASearchInput.FilterSingleValue();
                        ((JPASearchInput.FilterSingleValue) filter).setValue(e.getValue());
                    }

                    filter.setKey(field);
                    filter.setOperator(operator);
                    if (ignoreCase || negation) {
                        filter.setOptions(new JPASearchInput.JPASearchFilterOptions());
                        filter.getOptions().setIgnoreCase(ignoreCase);
                        filter.getOptions().setNegate(negation);
                    }

                    if (res.getFilter().getFilters() == null) {
                        res.getFilter().setFilters(new ArrayList<>());
                    }

                    res.getFilter().getFilters().add(filter);

                });

        return res;
    }

    public static Predicate[] toPredicates(Expression<Boolean>[] values) {
        Predicate[] predicates = new Predicate[values.length];
        for(int i = 0; i < values.length; i++) {
            predicates[i] = (Predicate) values[i];
        }
        return predicates;
    }

    public static void fetchManagement(Map<String, JoinType> fetchMap, Root<?> root) {

        if (fetchMap != null) {
            fetchMap = new TreeMap<>(fetchMap);
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
    }

    public static <T> Expression<T> getPath(Root<?> root, String k) {

        if (k.contains(".")) {

            Path<T> path = null;

            for (String f : k.split("\\.")) {
                path = path == null ? root.get(f) : path.get(f);
            }

            return path;

        } else {
            return root.get(k);
        }
    }

}
