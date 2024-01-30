package app.tozzi;

import app.tozzi.model.FieldRootBuilderBean;
import jakarta.persistence.criteria.*;

import java.util.Collection;
import java.util.function.Function;

public class JPASearchFunctions {

    public static final Function<FieldRootBuilderBean<?>, Predicate> EQ = s -> s.criteriaBuilder.equal(s.trim ? s.criteriaBuilder.trim(getPath(s.root, s.field)) : getPath(s.root, s.field), s.value);
    public static final Function<FieldRootBuilderBean<?>, Predicate> EQ_IGNORECASE = s -> s.criteriaBuilder.equal(s.trim ? s.criteriaBuilder.trim(s.criteriaBuilder.upper(getPath(s.root, s.field))) : s.criteriaBuilder.upper(getPath(s.root, s.field)), toUpperCase(s.value));
    public static final Function<FieldRootBuilderBean<?>, Predicate> STARTSWITH = s -> s.criteriaBuilder.like(s.trim ? s.criteriaBuilder.trim(getPath(s.root, s.field)) : getPath(s.root, s.field), s.value + "%");
    public static final Function<FieldRootBuilderBean<?>, Predicate> STARTSWITH_IGNORECASE = s -> s.criteriaBuilder.like(s.trim ? s.criteriaBuilder.trim(s.criteriaBuilder.upper(getPath(s.root, s.field))) : s.criteriaBuilder.upper(getPath(s.root, s.field)), toUpperCase(s.value) + "%");
    public static final Function<FieldRootBuilderBean<?>, Predicate> ENDSWITH = s -> s.criteriaBuilder.like(s.trim ? s.criteriaBuilder.trim(getPath(s.root, s.field)) : getPath(s.root, s.field), "%" + s.value);
    public static final Function<FieldRootBuilderBean<?>, Predicate> ENDSWITH_IGNORECASE = s -> s.criteriaBuilder.like(s.trim ? s.criteriaBuilder.trim(s.criteriaBuilder.upper(getPath(s.root, s.field))) : s.criteriaBuilder.upper(getPath(s.root, s.field)), "%" + toUpperCase(s.value));
    public static final Function<FieldRootBuilderBean<?>, Predicate> CONTAINS = s -> s.criteriaBuilder.like(getPath(s.root, s.field), "%" + s.value + "%");
    public static final Function<FieldRootBuilderBean<?>, Predicate> CONTAINS_IGNORECASE = s -> s.criteriaBuilder.like(s.criteriaBuilder.upper(getPath(s.root, s.field)), "%" + toUpperCase(s.value) + "%");
    public static final Function<FieldRootBuilderBean<?>, Predicate> NOTEQ = s -> s.criteriaBuilder.notEqual(s.trim ? s.criteriaBuilder.trim(getPath(s.root, s.field)) : getPath(s.root, s.field), s.value);
    public static final Function<FieldRootBuilderBean<?>, Predicate> NOTEQ_IGNORECASE = s -> s.criteriaBuilder.notEqual(s.trim ? s.criteriaBuilder.trim(s.criteriaBuilder.upper(getPath(s.root, s.field))) : s.criteriaBuilder.upper(getPath(s.root, s.field)), toUpperCase(s.value));
    public static final Function<FieldRootBuilderBean<?>, Predicate> GT = s -> s.criteriaBuilder.greaterThan(getPath(s.root, s.field), (Comparable) s.value);
    public static final Function<FieldRootBuilderBean<?>, Predicate> GTE = s -> s.criteriaBuilder.greaterThanOrEqualTo(getPath(s.root, s.field), (Comparable) s.value);
    public static final Function<FieldRootBuilderBean<?>, Predicate> LT = s -> s.criteriaBuilder.lessThan(getPath(s.root, s.field), (Comparable) s.value);
    public static final Function<FieldRootBuilderBean<?>, Predicate> LTE = s -> s.criteriaBuilder.lessThanOrEqualTo(getPath(s.root, s.field), (Comparable) s.value);
    public static final Function<FieldRootBuilderBean<?>, Predicate> IN = s -> {

        CriteriaBuilder.In<Object> in = s.criteriaBuilder.in(getPath(s.root, s.field));
        if (s.value instanceof Collection<?> coll) {
            for (Object obj : coll) {
                in.value(obj);
            }

        } else {
            in.value(s.value);
        }

        return in;
    };
    public static final Function<FieldRootBuilderBean<?>, Predicate> NIN = s -> s.criteriaBuilder.not(IN.apply(s));
    public static final Function<FieldRootBuilderBean<?>, Predicate> NOT_NULL = s -> s.criteriaBuilder.isNotNull(getPath(s.root, s.field));
    public static final Function<FieldRootBuilderBean<?>, Predicate> NOT_EMPTY = s -> s.criteriaBuilder.isNotEmpty(getPath(s.root, s.field));
    public static final Function<FieldRootBuilderBean<?>, Predicate> NULL = s -> s.criteriaBuilder.isNull(getPath(s.root, s.field));
    public static final Function<FieldRootBuilderBean<?>, Predicate> EMPTY = s -> s.criteriaBuilder.isEmpty(getPath(s.root, s.field));
    public static final Function<FieldRootBuilderBean<?>, Predicate> BETWEEN = s -> {
        Collection<?> coll = (Collection<?>) s.value;
        return s.criteriaBuilder.between(getPath(s.root, s.field), (Comparable) coll.iterator().next(), (Comparable) coll.iterator().next());
    };

    private static Object toUpperCase(Object object) {
        return object.toString().toUpperCase();
    }

    private static <T> Expression<T> getPath(Root<?> root, String k) {

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
