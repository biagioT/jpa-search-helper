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

            // Apply DISTINCT only when joins/fetches are actually present, since
            // an unconditional DISTINCT may slow down simple queries unnecessarily.
            if (!root.getJoins().isEmpty() || !root.getFetches().isEmpty()) {
                query.distinct(true);
            }

            if (expr == null) {
                return criteriaBuilder.conjunction();
            }

            if (expr instanceof Predicate predicate) {
                return predicate;
            }

            throw new JPASearchException("Not resulting a predicate: " + expr);
        };
    }

    public static Sort loadSort(JPASearchInput.JPASearchOptions options,
                                Map<String, Pair<Searchable, Field>> searchableFields,
                                Map<String, String> entityFieldMap) {
        return loadSort(options, searchableFields, entityFieldMap, false);
    }

    public static PageRequest loadSortAndPagination(JPASearchInput.JPASearchOptions options, Map<String, Pair<Searchable, Field>> searchableFields, Map<String, String> entityFieldMap) {
        if (options == null || options.getPageSize() == null || options.getPageSize() <= 0) {
            throw new JPASearchException("Invalid or not present page size value");
        }

        var result = PageRequest.ofSize(options.getPageSize()).withPage(options.getPageOffset() != null && options.getPageOffset() >= 0 ? options.getPageOffset() : 0);
        var sort = loadSort(options, searchableFields, entityFieldMap, true);

        return sort != null ? result.withSort(sort) : result;
    }

    private static Sort loadSort(JPASearchInput.JPASearchOptions options,
                                 Map<String, Pair<Searchable, Field>> searchableFields,
                                 Map<String, String> entityFieldMap, boolean nullable) {

        if (options == null || options.getSortOptions() == null || options.getSortOptions().isEmpty()) {
            if (nullable) return null;
            throw new JPASearchException("Invalid sort options");
        }

        var orders = new ArrayList<Sort.Order>();

        options.getSortOptions().forEach(so -> {
            var des = JPASearchCoreFieldProcessor.processField(so.getKey(), entityFieldMap, searchableFields, true, true, true);

            if (des == null) {
                if (nullable) return;
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
                return null;
            }

            return operator.getFunction().apply(cb, arguments.toArray(new Expression[0]), new Object[]{});
        }

        throw new JPASearchException("Invalid expression");
    }

    @SuppressWarnings("unchecked")
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
            var searchFilter = JPASearchOperatorFilter.load(fieldFilter.getOperator());
            var descriptor = JPASearchCoreFieldProcessor.processField(fieldFilter.getKey(), entityFieldMap, searchableFields, true, true, false);

            if (descriptor == null) {
                return null;
            }

            ValidationUtils.searchableValidations(descriptor.getSearchable(), descriptor.getPath(), searchFilter);

            var ignoreCase = fieldFilter.getOptions() != null && fieldFilter.getOptions().isIgnoreCase();
            var obj = new ArrayList<>();

            if (fieldFilter instanceof JPASearchInput.FilterSingleValue fsv) {
                var valueOpt = JPASearchCoreValueProcessor.processValue(searchFilter, descriptor.getSearchType(), descriptor.getSearchable(), descriptor.getPath(), fsv.getValue(), descriptor.getType(), ignoreCase);
                valueOpt.ifPresent(obj::add);

            } else if (fieldFilter instanceof JPASearchInput.FilterMultipleValues fmv) {
                var valueOpt = JPASearchCoreValueProcessor.processValue(searchFilter, descriptor.getSearchType(), descriptor.getSearchable(), descriptor.getPath(), fmv.getValues(), descriptor.getType(), ignoreCase);
                valueOpt.ifPresent(val -> {
                    if (val instanceof Collection<?> coll) {
                        obj.addAll(coll);
                    } else {
                        obj.add(val);
                    }
                });
            }

            if (obj.isEmpty() && searchFilter.getAllowedValues() != 0) {
                return null;
            }

            var isNullOrEmpty = searchFilter == JPASearchOperatorFilter.NULL || searchFilter == JPASearchOperatorFilter.EMPTY;

            if (descriptor.getSearchable().elementCollection() && (searchFilter == JPASearchOperatorFilter.EQ || searchFilter == JPASearchOperatorFilter.IN)) {
                var predicates = new ArrayList<Predicate>();
                var collectionPath = JPASearchUtils.getPath(root, descriptor.getEntityKey());
                var typedCollectionPath = (Expression<Collection<Object>>) (Expression<?>) collectionPath;

                for (var value : obj) {
                    predicates.add(cb.isMember(value, typedCollectionPath));
                }

                var finalPredicate = predicates.size() == 1 ? predicates.get(0) : cb.or(predicates.toArray(new Predicate[0]));

                return fieldFilter.getOptions() != null && fieldFilter.getOptions().isNegate()
                        ? cb.not(finalPredicate)
                        : finalPredicate;
            }

            // elementCollection + EMPTY/NULL: use cb.isEmpty on the collection expression itself,
            // avoiding the LEFT JOIN that would otherwise be generated by the generic path below.
            if (descriptor.getSearchable().elementCollection() && isNullOrEmpty) {
                var collectionPath = JPASearchUtils.getPath(root, descriptor.getEntityKey());
                @SuppressWarnings("unchecked")
                var typedCollectionPath = (Expression<? extends Collection<?>>) (Expression<?>) collectionPath;
                Predicate predicate = cb.isEmpty(typedCollectionPath);

                return fieldFilter.getOptions() != null && fieldFilter.getOptions().isNegate()
                        ? cb.not(predicate)
                        : predicate;
            }

            Expression<?> path;
            if (descriptor.getSearchable().elementCollection()) {
                path = root.join(descriptor.getEntityKey(), JoinType.LEFT);
            } else if (isNullOrEmpty && descriptor.getEntityKey().contains(".")) {
                // NULL/EMPTY on a nested path must traverse the relationship via LEFT JOIN,
                // otherwise the implicit inner join silently drops rows where the relation is null.
                path = buildLeftJoinPath(root, descriptor.getEntityKey());
            } else {
                path = JPASearchUtils.getPath(cb, root, descriptor.getEntityKey(), descriptor.getJsonPath());
            }

            var exps = new ArrayList<Expression<?>>();
            Expression<?> exp = null;
            var trim = false;

            if (descriptor.getSearchable().trim()) {
                exp = cb.trim(path.as(String.class));
                trim = true;
            }

            if (fieldFilter.getOptions() != null) {
                if (!trim && fieldFilter.getOptions().isTrim()) {
                    exp = cb.trim(path.as(String.class));
                }
                if (ignoreCase) {
                    exp = exp != null ? cb.lower(exp.as(String.class)) : cb.lower(path.as(String.class));
                }
            }

            exps.add(exp != null ? exp : path);

            var predicate = searchFilter.getFunction().apply(cb, exps.toArray(new Expression[0]), obj.toArray(new Object[0]));

            return fieldFilter.getOptions() != null && fieldFilter.getOptions().isNegate()
                    ? JPASearchOperatorGroup.NOT.getFunction().apply(cb, new Expression[]{predicate}, new Object[]{})
                    : predicate;
        }

        throw new JPASearchException("Invalid expression");
    }

    /**
     * Builds (and reuses, if already present) explicit LEFT JOINs for every intermediate segment
     * of a dotted entity path, returning the leaf {@link Expression}.
     * <p>
     * Used to ensure NULL/EMPTY semantics on optional relationships: with a plain implicit
     * inner-join chain ({@code root.get("a").get("b")}) the JPA provider would silently exclude
     * rows where {@code a} is null, defeating the {@code IS NULL} predicate.
     */
    private static Expression<?> buildLeftJoinPath(Root<?> root, String entityKey) {
        var segments = entityKey.split("\\.");
        From<?, ?> current = root;
        for (int i = 0; i < segments.length - 1; i++) {
            var segment = segments[i];
            var existing = current.getJoins().stream()
                    .filter(j -> j.getAttribute().getName().equals(segment))
                    .findAny();
            current = existing.isPresent() ? existing.get() : current.join(segment, JoinType.LEFT);
        }
        return current.get(segments[segments.length - 1]);
    }
}