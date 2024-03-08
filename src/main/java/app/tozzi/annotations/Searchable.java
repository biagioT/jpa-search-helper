package app.tozzi.annotations;

import app.tozzi.model.SearchType;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Searchable {

    String entityFieldKey() default "";

    SearchType targetType() default SearchType.UNTYPED;

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

}