package app.tozzi.core;

import app.tozzi.annotation.Searchable;
import app.tozzi.exception.JPASearchException;
import app.tozzi.model.JPASearchOperatorFilter;
import app.tozzi.model.JPASearchOperatorGroup;
import app.tozzi.model.input.JPASearchInput;
import app.tozzi.util.JPASearchUtils;
import app.tozzi.util.ValidationUtils;
import jakarta.persistence.criteria.*;
import lombok.AllArgsConstructor;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

@AllArgsConstructor
public class JPASearchCore {

    public static <R> Specification<R> specification(JPASearchInput.RootFilter filter,
                                              Map<String, Pair<Searchable, Class<?>>> searchableFields,
                                              Map<String, JoinType> fetchMap,
                                              Map<String, String> entityFieldMap) {


        return (root, query, criteriaBuilder) -> {
            var expr = processExpression(
                    filter,
                    criteriaBuilder,
                    JPASearchUtils.fetchManagement(fetchMap, root),
                    searchableFields,
                    entityFieldMap
            );

            if (expr instanceof Predicate predicate)
                return predicate;

            throw new JPASearchException("Not resulting a predicate" + expr);
        };
    }

    public static Sort loadSort(JPASearchInput.JPASearchOptions options,
                         Map<String, Pair<Searchable, Class<?>>> searchableFields,
                         Map<String, String> entityFieldMap) {

        return loadSort(options, searchableFields, entityFieldMap, false);

    }

    private static Sort loadSort(JPASearchInput.JPASearchOptions options,
                                 Map<String, Pair<Searchable, Class<?>>> searchableFields,
                                 Map<String, String> entityFieldMap, boolean nullable) {

        if (options == null || options.getSortKey() == null) {

            if (nullable)
                return null;

            throw new JPASearchException("Invalid sort key");
        }

        var des = JPASearchCoreFieldProcessor.processField(options.getSortKey(), entityFieldMap, searchableFields, true, true, true);

        if (des == null) {
            if (nullable)
                return null;

            throw new JPASearchException("Invalid sort key");
        }

        var sort = Sort.by(des.getEntityKey());
        sort = Boolean.TRUE.equals(options.getSortDesc()) ? sort.descending() : sort.ascending();
        return sort;
    }

    public static PageRequest loadSortAndPagination(JPASearchInput.JPASearchOptions options, Map<String, Pair<Searchable, Class<?>>> searchableFields, Map<String, String> entityFieldMap) {

        if (options == null || options.getPageSize() == null || options.getPageSize() <= 0) {
            throw new JPASearchException("Invalid or not present page size value");
        }

        var result = PageRequest.ofSize(options.getPageSize());
        if (options.getPageOffset() != null && options.getPageOffset() >= 0) {
            result = result.withPage(options.getPageOffset());
        }

        var sort = loadSort(options, searchableFields, entityFieldMap, true);
        if (sort != null) {
            result = result.withSort(sort);
        }

        return result;
    }

    private static Expression<?> processExpression(
            JPASearchInput.Filter filter,
            CriteriaBuilder cb,
            Root<?> root,
            Map<String, Pair<Searchable, Class<?>>> searchableFields,
            Map<String, String> entityFieldMap
    ) {

        if (filter instanceof JPASearchInput.RootFilter rootFilter) {

            if (rootFilter.getFilters() == null || rootFilter.getFilters().isEmpty()) {
                throw new JPASearchException("Invalid expression: empty filters");
            }

            var operator = JPASearchOperatorGroup.load(rootFilter.getOperator());
            var arguments = new ArrayList<>();
            for (JPASearchInput.Filter f : rootFilter.getFilters()) {
                var ex = process(f, cb, root, entityFieldMap, searchableFields);
                if (ex != null) {
                    arguments.add(ex);
                }
            }

            if (arguments.isEmpty()) {
                throw new JPASearchException("Invalid expression");
            }

            return operator.getFunction().apply(cb, arguments.toArray(new Expression[0]), new Object[]{});

        }

        throw new JPASearchException("Invalid expression");
    }

    private static Expression<?> process(
            JPASearchInput.Filter filter,
            CriteriaBuilder cb,
            Root<?> root,
            Map<String, String> entityFieldMap,
            Map<String, Pair<Searchable, Class<?>>> searchableFields
    ) {

        if (filter instanceof JPASearchInput.RootFilter) {
            return processExpression(filter, cb, root, searchableFields, entityFieldMap);

        } else if (filter instanceof JPASearchInput.FieldFilter fieldFilter) {
            var exps = new ArrayList<>();
            var searchFilter = JPASearchOperatorFilter.load(fieldFilter.getOperator());
            var descriptor = JPASearchCoreFieldProcessor.processField(fieldFilter.getKey(), entityFieldMap, searchableFields, true, true, false);

            if (descriptor == null) {
                return null;
            }

            ValidationUtils.searchableValidations(descriptor.getSearchable(), descriptor.getPath(), searchFilter);
            var path = JPASearchUtils.getPath(root, descriptor.getEntityKey());

            Expression<?> exp = null;
            var trim = false;
            var ignoreCase = false;

            if (descriptor.getSearchable().trim()) { // TODO  && SearchType.STRING.equals(descriptor.getSearchType())
                exp = cb.trim(path.as(String.class));
                trim = true;
            }

            if (!trim && fieldFilter.getOptions() != null && fieldFilter.getOptions().isTrim()) { // TODO  && SearchType.STRING.equals(descriptor.getSearchType())
                exp = cb.trim(path.as(String.class));
            }

            if (fieldFilter.getOptions() != null && fieldFilter.getOptions().isIgnoreCase()) { // TODO  && SearchType.STRING.equals(descriptor.getSearchType())
                ignoreCase = true;
                exp = exp != null ? cb.lower(exp.as(String.class)) : cb.lower(path.as(String.class));
            }

            exps.add(exp != null ? exp : path);

            var obj = new ArrayList<>();
            if (fieldFilter instanceof JPASearchInput.FilterSingleValue fsv) {
                var valueExp = JPASearchCoreValueProcessor.processValue(searchFilter, descriptor.getSearchType(), descriptor.getSearchable(), descriptor.getPath(), fsv.getValue(), ignoreCase);
                if (valueExp != null) {
                    obj.add(valueExp);
                }

            } else if (fieldFilter instanceof JPASearchInput.FilterMultipleValues fmv) {
                var valueExp = JPASearchCoreValueProcessor.processValue(searchFilter, descriptor.getSearchType(), descriptor.getSearchable(), descriptor.getPath(), fmv.getValues(), ignoreCase);
                if (valueExp != null) {
                    if (valueExp instanceof Collection<?> coll) {
                        obj.addAll(coll);

                    } else {
                        obj.add(valueExp);
                    }
                }
            }

            return fieldFilter.getOptions() != null
                    && fieldFilter.getOptions().isNegate()
                    ? JPASearchOperatorGroup.NOT.getFunction().apply(cb, new Expression[] {searchFilter.getFunction().apply(cb, exps.toArray(new Expression[0]), obj.toArray(new Object[0]))}, new Object[] {})
                    : searchFilter.getFunction().apply(cb, exps.toArray(new Expression[0]), obj.toArray(new Object[0]));
        }

        throw new JPASearchException("Invalid expression");
    }
}