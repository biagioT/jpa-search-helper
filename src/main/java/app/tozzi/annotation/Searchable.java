package app.tozzi.annotation;

import app.tozzi.model.JPASearchOperatorFilter;
import app.tozzi.model.JPASearchType;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Searchable {

    String entityFieldKey() default "";

    String jsonPath() default "";

    /**
     * The managed type for this searchable field.
     * <p>
     * When left as the default ({@link JPASearchType#UNTYPED}) the library infers the actual type from
     * the declared Java field type. {@code UNTYPED} is an internal sentinel value and should not be
     * used explicitly: setting it has the same effect as omitting the attribute.
     */
    JPASearchType targetType() default JPASearchType.UNTYPED;

    String datePattern() default "";

    int maxSize() default -1;

    int minSize() default -1;

    String regexPattern() default "";

    int maxDigits() default -1;

    int minDigits() default -1;

    String decimalFormat() default "#.##";

    boolean sortable() default true;

    boolean trim() default false;

    Tag[] tags() default {};

    JPASearchOperatorFilter[] allowedFilters() default {};

    JPASearchOperatorFilter[] notAllowedFilters() default {};

    boolean allowLikeFilters() default true;

    boolean ordinalEnum() default false;

    /**
     * Set to {@code true} when the underlying entity attribute is a {@link jakarta.persistence.ElementCollection}.
     * <p>
     * With this flag enabled the {@code EQ} and {@code IN} operators are evaluated with
     * {@code cb.isMember(...)} on the collection expression instead of joining the collection table.
     * All other operators (CONTAINS, STARTS_WITH, ENDS_WITH, ...) currently fall back to a {@code LEFT JOIN}
     * on the element-collection table; this typically requires a {@code DISTINCT} clause to avoid
     * duplicate rows in the result.
     */
    boolean elementCollection() default false;
}
