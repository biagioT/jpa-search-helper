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

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Objects;

@AllArgsConstructor
public class JPASearchCore {

    /**
     * Generates a {@link Specification} from the search filters
     *
     * @param filter
     * @param searchableFields
     * @param fetchMap
     * @param entityFieldMap
     * @param <R>
     * @return
     */
    public static <R> Specification<R> specification(JPASearchInput.RootFilter filter,
                                                     Map<String, Pair<Searchable, Field>> searchableFields,
                                                     Map<String, JoinType> fetchMap,
                                                     Map<String, String> entityFieldMap) {

        if (filter == null) {
            return (root, query, cb) -> cb.conjunction();
        }

        return (root, query, criteriaBuilder) -> {
            var expr = processExpression(
                    filter,
                    criteriaBuilder,
                    JPASearchUtils.fetchManagement(fetchMap, root),
                    searchableFields,
                    entityFieldMap
            );

            if (expr == null) {
                return criteriaBuilder.conjunction();
            }

            if (expr instanceof Predicate predicate) {
                return predicate;
            }

            throw new JPASearchException("Not resulting a predicate: " + expr);
        };
    }

    /**
     * Generate sorting parameters
     *
     * @param options
     * @param searchableFields
     * @param entityFieldMap
     * @return
     */
    public static Sort loadSort(JPASearchInput.JPASearchOptions options,
                                Map<String, Pair<Searchable, Field>> searchableFields,
                                Map<String, String> entityFieldMap) {

        return loadSort(options, searchableFields, entityFieldMap, false);

    }

    /**
     * Generate pagination parameters
     *
     * @param options
     * @param searchableFields
     * @param entityFieldMap
     * @return
     */
    public static PageRequest loadSortAndPagination(JPASearchInput.JPASearchOptions options, Map<String, Pair<Searchable, Field>> searchableFields, Map<String, String> entityFieldMap) {

        if (options == null || options.getPageSize() == null || options.getPageSize() <= 0) {
            throw new JPASearchException("Invalid or not present page size value");
        }

        var result = PageRequest.ofSize(options.getPageSize()).withPage(options.getPageOffset() != null && options.getPageOffset() >= 0 ? options.getPageOffset() : 0);
        var sort = loadSort(options, searchableFields, entityFieldMap, true);

        return sort != null
                ? result.withSort(sort)
                : result;
    }

    private static Sort loadSort(JPASearchInput.JPASearchOptions options,
                                 Map<String, Pair<Searchable, Field>> searchableFields,
                                 Map<String, String> entityFieldMap, boolean nullable) {

        if (options == null || options.getSortOptions() == null || options.getSortOptions().isEmpty()) {

            if (nullable)
                return null;

            throw new JPASearchException("Invalid sort options");
        }

        var orders = new ArrayList<Sort.Order>();

        options.getSortOptions().forEach(so -> {
            var des = JPASearchCoreFieldProcessor.processField(so.getKey(), entityFieldMap, searchableFields, true, true, true);

            if (des == null) {
                if (nullable)
                    return;

                throw new JPASearchException("Invalid sort key");
            }

            orders.add(Boolean.TRUE.equals(so.getDesc()) ? Sort.Order.desc(des.getEntityKey()) : Sort.Order.asc(des.getEntityKey()));
        });


        return Sort.by(orders);
    }

    private static Expression<?> processExpression(
            JPASearchInput.Filter filter,
            CriteriaBuilder cb,
            Root<?> root,
            Map<String, Pair<Searchable, Field>> searchableFields,
            Map<String, String> entityFieldMap
    ) {

        if (filter instanceof JPASearchInput.RootFilter rootFilter) {

            if (rootFilter.getFilters() == null || rootFilter.getFilters().isEmpty()) {
                return null;
            }

            var operator = JPASearchOperatorGroup.load(rootFilter.getOperator());
            var arguments = rootFilter.getFilters().stream()
                    .map(f -> process(f, cb, root, entityFieldMap, searchableFields))
                    .filter(Objects::nonNull)
                    .toList();

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
            Map<String, Pair<Searchable, Field>> searchableFields
    ) {

        if (filter instanceof JPASearchInput.RootFilter) {
            return processExpression(filter, cb, root, searchableFields, entityFieldMap);
        }

        if (filter instanceof JPASearchInput.FieldFilter fieldFilter) {
            var exps = new ArrayList<>();
            var searchFilter = JPASearchOperatorFilter.load(fieldFilter.getOperator());
            var descriptor = JPASearchCoreFieldProcessor.processField(fieldFilter.getKey(), entityFieldMap, searchableFields, true, true, false);

            if (descriptor == null) {
                return null;
            }

            ValidationUtils.searchableValidations(descriptor.getSearchable(), descriptor.getPath(), searchFilter);
            var path = JPASearchUtils.getPath(cb, root, descriptor.getEntityKey(), descriptor.getJsonPath());

            Expression<?> exp = null;
            var trim = false;
            var ignoreCase = false;

            if (descriptor.getSearchable().trim()) {
                exp = cb.trim(path.as(String.class));
                trim = true;
            }

            if (fieldFilter.getOptions() != null) {

                if (!trim && fieldFilter.getOptions().isTrim()) {
                    exp = cb.trim(path.as(String.class));
                }

                if (fieldFilter.getOptions().isIgnoreCase()) {
                    ignoreCase = true;
                    exp = exp != null ? cb.lower(exp.as(String.class)) : cb.lower(path.as(String.class));
                }
            }

            exps.add(exp != null ? exp : path);

            var obj = new ArrayList<>();
            if (fieldFilter instanceof JPASearchInput.FilterSingleValue fsv) {
                var valueExp = JPASearchCoreValueProcessor.processValue(searchFilter, descriptor.getSearchType(), descriptor.getSearchable(), descriptor.getPath(), fsv.getValue(), descriptor.getType(), ignoreCase);
                if (valueExp != null) {
                    obj.add(valueExp);
                }

            } else if (fieldFilter instanceof JPASearchInput.FilterMultipleValues fmv) {
                var valueExp = JPASearchCoreValueProcessor.processValue(searchFilter, descriptor.getSearchType(), descriptor.getSearchable(), descriptor.getPath(), fmv.getValues(), descriptor.getType(), ignoreCase);
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
                    ? JPASearchOperatorGroup.NOT.getFunction().apply(cb, new Expression[]{searchFilter.getFunction().apply(cb, exps.toArray(new Expression[0]), obj.toArray(new Object[0]))}, new Object[]{})
                    : searchFilter.getFunction().apply(cb, exps.toArray(new Expression[0]), obj.toArray(new Object[0]));
        }

        throw new JPASearchException("Invalid expression");
    }
}