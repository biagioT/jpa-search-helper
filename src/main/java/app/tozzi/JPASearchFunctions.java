package app.tozzi;

import app.tozzi.exceptions.JPASearchException;
import app.tozzi.utils.JPAExpressionFunction;
import app.tozzi.utils.JPAExpressionFunction2;
import app.tozzi.utils.ReflectionUtils;

import javax.persistence.criteria.*;

import java.math.BigDecimal;
import java.sql.Date;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collection;

public class JPASearchFunctions {
    public static final JPAExpressionFunction<Boolean, Boolean> AND = (cb, values, entityClass) -> cb.and(toPredicates(values));
    public static final JPAExpressionFunction<Boolean, Boolean> OR = (cb, values, entityClass) -> cb.or(toPredicates(values));
    public static final JPAExpressionFunction<Boolean, Boolean> NOT = (cb, values, entityClass) -> cb.not(values[0]);

    public static final JPAExpressionFunction<?, Boolean> EQ = (cb, values, entityClass) -> cb.equal(values[0], values[1]);
    public static final JPAExpressionFunction<String, Boolean> EQ_IGNORECASE
        = (cb, values, entityClass) -> cb.equal(cb.upper(values[0]), cb.upper(values[1]));
    public static final JPAExpressionFunction<String, Boolean> STARTSWITH
        = (cb, values, entityClass) -> cb.like(values[0], cb.concat(values[1],  "%"));
    public static final JPAExpressionFunction<String, Boolean> STARTSWITH_IGNORECASE
        = (cb, values, entityClass) -> cb.like(cb.upper(values[0]), cb.concat(cb.upper(values[1]), "%"));
    public static final JPAExpressionFunction<String, Boolean> ENDSWITH
        = (cb, values, entityClass) -> cb.like(values[0], cb.concat("%", values[1]));
    public static final JPAExpressionFunction<String, Boolean> ENDSWITH_IGNORECASE
        = (cb, values, entityClass) -> cb.like(cb.upper(values[0]), cb.concat("%", cb.upper(values[1])));
    public static final JPAExpressionFunction<String, Boolean> CONTAINS
        = (cb, values, entityClass) -> cb.like(values[0], cb.concat(cb.concat("%", values[1]), "%"));
    public static final JPAExpressionFunction<String, Boolean> CONTAINS_IGNORECASE
        = (cb, values, entityClass) -> cb.like(cb.upper(values[0]), cb.concat("%", cb.concat(cb.upper(values[1]), "%")));
    public static final JPAExpressionFunction<?, Boolean> NOTEQ
        = (cb, values, entityClass) -> cb.notEqual(values[0], values[1]);
    public static final JPAExpressionFunction<String, Boolean> NOTEQ_IGNORECASE
        = (cb, values, entityClass) -> cb.notEqual(cb.upper(values[0]), cb.upper(values[1]));
    public static final JPAExpressionFunction<Comparable, Boolean> GT
        = (cb, values, entityClass) -> cb.greaterThan(values[0], values[1]);
    public static final JPAExpressionFunction<Comparable, Boolean> GTE
        = (cb, values, entityClass) -> cb.greaterThanOrEqualTo(values[0], values[1]);
    public static final JPAExpressionFunction<Comparable, Boolean> LT
        = (cb, values, entityClass) -> cb.lessThan(values[0], values[1]);
    public static final JPAExpressionFunction<Comparable, Boolean> LTE
        = (cb, values, entityClass) -> cb.lessThanOrEqualTo(values[0], values[1]);
    public static final JPAExpressionFunction<Collection, Boolean> IN = (cb, values, entityClass) -> {
        CriteriaBuilder.In<Object> in = cb.in(values[0]);
        var size = values.length;
        for (var i = 1; i < size; i++) {
            in.value(values[i]);
        }
        return in;
    };
    public static final JPAExpressionFunction<Collection, Boolean> NIN
        = (cb, values, entityClass) -> cb.not(IN.apply(cb, values, entityClass));
    public static final JPAExpressionFunction<?, Boolean> NOT_NULL
        = (cb, values, entityClass) -> cb.isNotNull(values[0]);
    public static final JPAExpressionFunction<Collection, Boolean> NOT_EMPTY
        = (cb, values, entityClass) -> cb.isNotEmpty(values[0]);
    public static final JPAExpressionFunction<?, Boolean> NULL
        = (cb, values, entityClass) -> cb.isNull(values[0]);
    public static final JPAExpressionFunction<Collection, Boolean> EMPTY
        = (cb, values, entityClass) -> cb.isEmpty(values[0]);
    public static final JPAExpressionFunction<Comparable, Boolean> BETWEEN = (cb, values, entityClass) -> cb.between(values[0], values[1], values[2]);

    public static final JPAExpressionFunction2<Date> DATE = (cb, values, entityClass) -> {
        var dateStr = ZonedDateTime.parse((String)values[0]).withZoneSameInstant(ZoneOffset.UTC).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME);
        return cb.function("STR_TO_DATE", java.sql.Date.class, cb.literal(dateStr), cb.literal("%Y-%m-%dT%H:%i:%sZ"));
    };

    public static final JPAExpressionFunction2<BigDecimal> BIG_DECIMAL = (cb, values, entityClass) -> cb.literal(new BigDecimal((String)values[0]));

    public static final JPAExpressionFunction2<String> STR = (cb, values, entityClass) -> cb.literal((String)values[0]);

    public static final JPAExpressionFunction2<Enum> ENUM = (cb, values, entityClass) -> {
        var className = (String)values[0];
        var valueName = (String)values[1];
        var searchables = ReflectionUtils.getAllSearchableFields(entityClass);
        var cls = (Class<Enum>) searchables.values().stream().filter(v -> v.getValue().isEnum() && v.getValue().getName().endsWith("." + className)).findFirst().orElseThrow(() -> new JPASearchException("Enum not found")).getValue();
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