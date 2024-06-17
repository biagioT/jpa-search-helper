package app.tozzi;

import app.tozzi.utils.JPAExpressionFunction;

import javax.persistence.criteria.*;

import java.math.BigDecimal;
import java.sql.Date;
import java.util.Collection;

public class JPASearchFunctions {
    public static final JPAExpressionFunction<Boolean, Boolean> AND = (cb, values) -> cb.and(toPredicates(values));
    public static final JPAExpressionFunction<Boolean, Boolean> OR = (cb, values) -> cb.or(toPredicates(values));
    public static final JPAExpressionFunction<Boolean, Boolean> NOT = (cb, values) -> cb.not(values[0]);

    public static final JPAExpressionFunction<?, Boolean> EQ = (cb, values) -> cb.equal(values[0], values[1]);
    public static final JPAExpressionFunction<String, Boolean> EQ_IGNORECASE
        = (cb, values) -> cb.equal(cb.upper(values[0]), cb.upper(values[1]));
    public static final JPAExpressionFunction<String, Boolean> STARTSWITH
        = (cb, values) -> cb.like(values[0], cb.concat(values[1],  "%"));
    public static final JPAExpressionFunction<String, Boolean> STARTSWITH_IGNORECASE
        = (cb, values) -> cb.like(cb.upper(values[0]), cb.concat(cb.upper(values[1]), "%"));
    public static final JPAExpressionFunction<String, Boolean> ENDSWITH
        = (cb, values) -> cb.like(values[0], cb.concat("%", values[1]));
    public static final JPAExpressionFunction<String, Boolean> ENDSWITH_IGNORECASE
        = (cb, values) -> cb.like(cb.upper(values[0]), cb.concat("%", cb.upper(values[1])));
    public static final JPAExpressionFunction<String, Boolean> CONTAINS
        = (cb, values) -> cb.like(values[0], cb.concat(cb.concat("%", values[1]), "%"));
    public static final JPAExpressionFunction<String, Boolean> CONTAINS_IGNORECASE
        = (cb, values) -> cb.like(cb.upper(values[0]), cb.concat("%", cb.concat(cb.upper(values[1]), "%")));
    public static final JPAExpressionFunction<?, Boolean> NOTEQ
        = (cb, values) -> cb.notEqual(values[0], values[1]);
    public static final JPAExpressionFunction<String, Boolean> NOTEQ_IGNORECASE
        = (cb, values) -> cb.notEqual(cb.upper(values[0]), cb.upper(values[1]));
    public static final JPAExpressionFunction<Comparable , Boolean> GT
        = (cb, values) -> cb.greaterThan(values[0], values[1]);
    public static final JPAExpressionFunction<Comparable, Boolean> GTE
        = (cb, values) -> cb.greaterThanOrEqualTo(values[0], values[1]);
    public static final JPAExpressionFunction<Comparable, Boolean> LT
        = (cb, values) -> cb.lessThan(values[0], values[1]);
    public static final JPAExpressionFunction<Comparable, Boolean> LTE
        = (cb, values) -> cb.lessThanOrEqualTo(values[0], values[1]);
    public static final JPAExpressionFunction<Collection, Boolean> IN = (cb, values) -> {
        CriteriaBuilder.In<Object> in = cb.in(values[0]);
        var size = values.length;
        for (var i = 1; i < size; i++) {
            in.value(values[i]);
        }
        return in;
    };
    public static final JPAExpressionFunction<Collection, Boolean> NIN
        = (cb, values) -> cb.not(IN.apply(cb, values));
    public static final JPAExpressionFunction<?, Boolean> NOT_NULL
        = (cb, values) -> cb.isNotNull(values[0]);
    public static final JPAExpressionFunction<Collection, Boolean> NOT_EMPTY
        = (cb, values) -> cb.isNotEmpty(values[0]);
    public static final JPAExpressionFunction<?, Boolean> NULL
        = (cb, values) -> cb.isNull(values[0]);
    public static final JPAExpressionFunction<Collection, Boolean> EMPTY
        = (cb, values) -> cb.isEmpty(values[0]);
    public static final JPAExpressionFunction<Comparable, Boolean> BETWEEN = (cb, values) -> cb.between(values[0], values[1], values[2]);

    public static final JPAExpressionFunction<Integer, Date> DATE = (cb, values) -> {
        var all = cb.concat(
            cb.concat(
                cb.concat(values[0].as(String.class), "-"),
                cb.concat(values[1].as(String.class), "-")
            ),
            values[2].as(String.class)
        );
        return cb.function("DATE", Date.class, all);
    };

    public static final JPAExpressionFunction<Integer, BigDecimal> BIG_DECIMAL = (cb, values) -> values[0].as(BigDecimal.class);

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
    public static Predicate[] toPredicates(Expression<Boolean>[] values) {
        Predicate[] predicates = new Predicate[values.length];
        for(int i = 0; i < values.length; i++) {
            predicates[i] = (Predicate) values[i];
        }
        return predicates;
    }
}