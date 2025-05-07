package com.gisgro;

import com.gisgro.exceptions.JPASearchException;
import com.gisgro.utils.JPAFuncWithExpressions;
import com.gisgro.utils.JPAFuncWithObjects;

import javax.persistence.criteria.*;
import java.math.BigDecimal;
import java.sql.Date;
import java.time.Period;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Collection;

public class JPASearchFunctions {
    public static final JPAFuncWithExpressions<Boolean, Boolean> AND = (cb, values) -> cb.and(toPredicates(values));
    public static final JPAFuncWithExpressions<Boolean, Boolean> OR = (cb, values) -> cb.or(toPredicates(values));
    public static final JPAFuncWithExpressions<Boolean, Boolean> NOT = (cb, values) -> cb.not(values[0]);

    public static final JPAFuncWithExpressions<?, Boolean> EQ = (cb, values) -> cb.equal(values[0], values[1]);
    public static final JPAFuncWithExpressions<String, Boolean> STARTSWITH
            = (cb, values) -> cb.like(values[0], cb.concat(values[1], "%"));
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

    public static final JPAFuncWithObjects<Date> DATE = (root, query, cb, values, entityClass) -> {
        var dateStr = ZonedDateTime.parse((String) values[0]).withZoneSameInstant(ZoneOffset.UTC).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME);
        return cb.function("STR_TO_DATE", java.sql.Date.class, cb.literal(dateStr), cb.literal("%Y-%m-%dT%H:%i:%sZ"));
    };

    public static final JPAFuncWithObjects<BigDecimal> BIG_DECIMAL = (root, query, cb, values, searchableFields) -> cb.literal(new BigDecimal((String) values[0]));

    public static final JPAFuncWithObjects<Period> PERIOD = (root, query, cb, values, searchableFields) -> cb.literal(Period.parse((String) values[0]));

    public static final JPAFuncWithExpressions<?, ?> FIELD = (cb, values) -> values[0]; // no-op, handled in processValue

    public static final JPAFuncWithObjects<Enum> ENUM = (root, query, cb, values, searchableFields) -> {
        var className = (String) values[0];
        var valueName = (String) values[1];
        var cls = (Class<Enum>) searchableFields.values()
                .stream()
                .filter(v -> v.getValue().isEnum() && v.getValue().getName().endsWith("." + className))
                .findFirst()
                .orElseThrow(() -> new JPASearchException("Enum not found"))
                .getValue();
        return cb.literal(Enum.valueOf(cls, valueName));
    };

    public static <Z, X> Expression<Z> getPath(Root<?> root, String k) {
        if (k.contains(".")) {
            Join<Z, X> path = null;
            var it = Arrays.stream(k.split("\\.")).iterator();
            var f = it.next();
            while (it.hasNext()) {
                path = path == null ? root.join(f, JoinType.LEFT) : path.join(f, JoinType.LEFT);
                f = it.next();
            }
            return path.get(f);
        } else {
            return root.get(k);
        }
    }

    public static Predicate[] toPredicates(Expression<Boolean>[] values) {
        Predicate[] predicates = new Predicate[values.length];
        for (int i = 0; i < values.length; i++) {
            predicates[i] = (Predicate) values[i];
        }
        return predicates;
    }
}