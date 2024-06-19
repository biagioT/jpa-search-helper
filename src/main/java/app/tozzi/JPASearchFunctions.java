package app.tozzi;

import app.tozzi.exceptions.JPASearchException;
import app.tozzi.utils.JPAFuncWithExpressions;
import app.tozzi.utils.JPAFuncWithObjects;
import app.tozzi.utils.ReflectionUtils;

import javax.persistence.criteria.*;

import java.math.BigDecimal;
import java.sql.Date;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collection;

public class JPASearchFunctions {
    public static final JPAFuncWithExpressions<Boolean, Boolean> AND = (cb, values) -> cb.and(toPredicates(values));
    public static final JPAFuncWithExpressions<Boolean, Boolean> OR = (cb, values) -> cb.or(toPredicates(values));
    public static final JPAFuncWithExpressions<Boolean, Boolean> NOT = (cb, values) -> cb.not(values[0]);

    public static final JPAFuncWithExpressions<?, Boolean> EQ = (cb, values) -> cb.equal(values[0], values[1]);
    public static final JPAFuncWithExpressions<String, Boolean> STARTSWITH
        = (cb, values) -> cb.like(values[0], cb.concat(values[1],  "%"));
    public static final JPAFuncWithExpressions<String, Boolean> ENDSWITH
        = (cb, values) -> cb.like(values[0], cb.concat("%", values[1]));
    public static final JPAFuncWithExpressions<String, Boolean> CONTAINS
        = (cb, values) -> cb.like(values[0], cb.concat(cb.concat("%", values[1]), "%"));
    public static final JPAFuncWithExpressions<Comparable, Boolean> GT
        = (cb, values) -> cb.greaterThan(values[0], values[1]);
    public static final JPAFuncWithExpressions<Comparable, Boolean> GTE
        = (cb, values) -> cb.greaterThanOrEqualTo(values[0], values[1]);
    public static final JPAFuncWithExpressions<Comparable, Boolean> LT
        = (cb, values) -> cb.lessThan(values[0], values[1]);
    public static final JPAFuncWithExpressions<Comparable, Boolean> LTE
        = (cb, values) -> cb.lessThanOrEqualTo(values[0], values[1]);
    public static final JPAFuncWithExpressions<Collection, Boolean> IN = (cb, values) -> {
        CriteriaBuilder.In<Object> in = cb.in(values[0]);
        var size = values.length;
        for (var i = 1; i < size; i++) {
            in.value(values[i]);
        }
        return in;
    };
    public static final JPAFuncWithExpressions<?, Boolean> NULL
        = (cb, values) -> cb.isNull(values[0]);
    public static final JPAFuncWithExpressions<Collection, Boolean> EMPTY
        = (cb, values) -> cb.isEmpty(values[0]);
    public static final JPAFuncWithExpressions<Comparable, Boolean> BETWEEN = (cb, values) -> cb.between(values[0], values[1], values[2]);

    public static final JPAFuncWithExpressions<String, String> LOWER = (cb, values) -> cb.lower(values[0]);

    public static final JPAFuncWithObjects<Date> DATE = (cb, values, entityClass) -> {
        var dateStr = ZonedDateTime.parse((String)values[0]).withZoneSameInstant(ZoneOffset.UTC).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME);
        return cb.function("STR_TO_DATE", java.sql.Date.class, cb.literal(dateStr), cb.literal("%Y-%m-%dT%H:%i:%sZ"));
    };


    public static final JPAFuncWithObjects<BigDecimal> BIG_DECIMAL = (cb, values, entityClass) -> cb.literal(new BigDecimal((String)values[0]));

    public static final JPAFuncWithObjects<String> STR = (cb, values, entityClass) -> cb.literal((String)values[0]);

    public static final JPAFuncWithObjects<Enum> ENUM = (cb, values, entityClass) -> {
        var className = (String)values[0];
        var valueName = (String)values[1];
        var searchables = ReflectionUtils.getAllSearchableFields(entityClass);
        var cls = (Class<Enum>) searchables.values()
            .stream()
            .filter(v -> v.getValue().isEnum() && v.getValue().getName().endsWith("." + className))
            .findFirst()
            .orElseThrow(() -> new JPASearchException("Enum not found"))
            .getValue();
        return cb.literal(Enum.valueOf(cls, valueName));
    };

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