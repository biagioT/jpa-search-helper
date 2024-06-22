package app.tozzi.function;

import app.tozzi.model.JPASearchFunction;
import app.tozzi.util.JPASearchUtils;
import jakarta.persistence.criteria.CriteriaBuilder;

import java.util.Collection;

public class JPASearchFunctions {

    public static final JPASearchFunction<Boolean, Boolean> AND = (cb, expressions) -> cb.and(JPASearchUtils.toPredicates(expressions));
    public static final JPASearchFunction<Boolean, Boolean> OR = (cb, expressions) -> cb.or(JPASearchUtils.toPredicates(expressions));
    public static final JPASearchFunction<Boolean, Boolean> NOT = (cb, expressions) -> cb.not(expressions[0]);

    public static final JPASearchFunction<?, Boolean> EQ = (cb, expressions) -> cb.equal(expressions[0], expressions[1]);
    public static final JPASearchFunction<String, Boolean> STARTSWITH = (cb, expressions) -> cb.like(expressions[0], cb.concat(expressions[1], "%"));
    public static final JPASearchFunction<String, Boolean> ENDSWITH = (cb, expressions) -> cb.like(expressions[0], cb.concat("%", expressions[1]));
    public static final JPASearchFunction<String, Boolean> CONTAINS = (cb, expressions) -> cb.like(expressions[0], cb.concat(cb.concat("%", expressions[1]), "%"));
    public static final JPASearchFunction<Comparable, Boolean> GT = (cb, expressions) -> cb.greaterThan(expressions[0], expressions[1]);
    public static final JPASearchFunction<Comparable, Boolean> GTE = (cb, expressions) -> cb.greaterThanOrEqualTo(expressions[0], expressions[1]);
    public static final JPASearchFunction<Comparable, Boolean> LT = (cb, expressions) -> cb.lessThan(expressions[0], expressions[1]);
    public static final JPASearchFunction<Comparable, Boolean> LTE = (cb, expressions) -> cb.lessThanOrEqualTo(expressions[0], expressions[0]);
    public static final JPASearchFunction<Comparable, Boolean> BETWEEN = (cb, expressions) -> cb.between(expressions[0], expressions[1], expressions[1]);
    public static final JPASearchFunction<?, Boolean> NULL = (cb, expressions) -> cb.isNull(expressions[0]);
    public static final JPASearchFunction<Collection, Boolean> EMPTY = (cb, expressions) -> cb.isEmpty(expressions[0]);

    public static final JPASearchFunction<Collection, Boolean> IN = (cb, expressions) -> {
        CriteriaBuilder.In<Object> in = cb.in(expressions[0]);
        var size = expressions.length;
        for (var i = 1; i < size; i++) {
            in.value(expressions[i]);
        }
        return in;
    };


}
