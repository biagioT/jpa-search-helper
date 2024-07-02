package app.tozzi.function;

import app.tozzi.model.JPASearchFunction;
import app.tozzi.util.JPASearchUtils;
import jakarta.persistence.criteria.CriteriaBuilder;

import java.util.Collection;

public class JPASearchFunctions {

    public static final JPASearchFunction<Boolean, Boolean> AND = (cb, expressions, values) -> cb.and(JPASearchUtils.toPredicates(expressions));
    public static final JPASearchFunction<Boolean, Boolean> OR = (cb, expressions, values) -> cb.or(JPASearchUtils.toPredicates(expressions));
    public static final JPASearchFunction<Boolean, Boolean> NOT = (cb, expressions, values) -> cb.not(expressions[0]);

    public static final JPASearchFunction<?, Boolean> EQ = (cb, expressions, values) -> cb.equal(expressions[0], values[0]);
    public static final JPASearchFunction<String, Boolean> STARTSWITH = (cb, expressions, values) -> cb.like(expressions[0], values[0] + "%");
    public static final JPASearchFunction<String, Boolean> ENDSWITH = (cb, expressions, values) -> cb.like(expressions[0],"%" + values[0]);
    public static final JPASearchFunction<String, Boolean> CONTAINS = (cb, expressions, values) -> cb.like(expressions[0], "%" + values[0] + "%");
    public static final JPASearchFunction<Comparable, Boolean> GT = (cb, expressions, values) -> cb.greaterThan(expressions[0], (Comparable) values[0]);
    public static final JPASearchFunction<Comparable, Boolean> GTE = (cb, expressions, values) -> cb.greaterThanOrEqualTo(expressions[0], (Comparable) values[0]);
    public static final JPASearchFunction<Comparable, Boolean> LT = (cb, expressions, values) -> cb.lessThan(expressions[0], (Comparable) values[0]);
    public static final JPASearchFunction<Comparable, Boolean> LTE = (cb, expressions, values) -> cb.lessThanOrEqualTo(expressions[0], (Comparable) values[0]);
    public static final JPASearchFunction<Comparable, Boolean> BETWEEN = (cb, expressions, values) -> cb.between(expressions[0], (Comparable) values[0], (Comparable) values[1]);
    public static final JPASearchFunction<?, Boolean> NULL = (cb, expressions, values) -> cb.isNull(expressions[0]);
    public static final JPASearchFunction<Collection, Boolean> EMPTY = (cb, expressions, values) -> cb.isEmpty(expressions[0]);

    public static final JPASearchFunction<Collection, Boolean> IN = (cb, expressions, values) -> {
        CriteriaBuilder.In<Object> in = cb.in(expressions[0]);
        var size = values.length;
        for (var i = 0; i < size; i++) {
            in.value(values[i]);
        }
        return in;
    };


}
