package app.tozzi.util;

import app.tozzi.annotation.Searchable;
import app.tozzi.annotation.Tag;
import app.tozzi.exception.InvalidFieldException;
import app.tozzi.model.JPASearchOperatorFilter;
import app.tozzi.model.JPASearchType;
import org.junit.jupiter.api.Test;

import java.lang.annotation.Annotation;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class ValidationUtilsTest {

    @Test
    public void notAllowedFilterTest() {
        assertThrows(InvalidFieldException.class,
                () -> ValidationUtils.searchableValidations(getSearchableInstance(new JPASearchOperatorFilter[]{JPASearchOperatorFilter.EQ},
                        new JPASearchOperatorFilter[]{JPASearchOperatorFilter.BETWEEN}, true), "field", JPASearchOperatorFilter.BETWEEN));

        assertThrows(InvalidFieldException.class,
                () -> ValidationUtils.searchableValidations(getSearchableInstance(new JPASearchOperatorFilter[]{JPASearchOperatorFilter.EQ},
                        new JPASearchOperatorFilter[]{JPASearchOperatorFilter.BETWEEN}, true), "field", JPASearchOperatorFilter.IN));

        assertThrows(InvalidFieldException.class,
                () -> ValidationUtils.searchableValidations(getSearchableInstance(new JPASearchOperatorFilter[]{JPASearchOperatorFilter.EQ},
                        new JPASearchOperatorFilter[]{JPASearchOperatorFilter.EQ}, true), "field", JPASearchOperatorFilter.EQ));

        assertThrows(InvalidFieldException.class,
                () -> ValidationUtils.searchableValidations(getSearchableInstance(new JPASearchOperatorFilter[]{JPASearchOperatorFilter.EQ},
                        new JPASearchOperatorFilter[]{JPASearchOperatorFilter.EQ}, true), "field", JPASearchOperatorFilter.EQ));

        assertThrows(InvalidFieldException.class,
                () -> ValidationUtils.searchableValidations(getSearchableInstance(new JPASearchOperatorFilter[]{JPASearchOperatorFilter.EQ},
                        new JPASearchOperatorFilter[]{JPASearchOperatorFilter.BETWEEN, JPASearchOperatorFilter.GT}, true), "field", JPASearchOperatorFilter.BETWEEN));

        assertThrows(InvalidFieldException.class,
                () -> ValidationUtils.searchableValidations(getSearchableInstance(new JPASearchOperatorFilter[]{JPASearchOperatorFilter.EQ, JPASearchOperatorFilter.CONTAINS},
                        new JPASearchOperatorFilter[]{JPASearchOperatorFilter.BETWEEN, JPASearchOperatorFilter.GT}, true), "field", JPASearchOperatorFilter.BETWEEN));

        assertThrows(InvalidFieldException.class,
                () -> ValidationUtils.searchableValidations(getSearchableInstance(new JPASearchOperatorFilter[]{JPASearchOperatorFilter.EQ, JPASearchOperatorFilter.CONTAINS},
                        new JPASearchOperatorFilter[]{}, true), "field", JPASearchOperatorFilter.BETWEEN));

        assertThrows(InvalidFieldException.class,
                () -> ValidationUtils.searchableValidations(getSearchableInstance(new JPASearchOperatorFilter[]{JPASearchOperatorFilter.EQ, JPASearchOperatorFilter.CONTAINS},
                        new JPASearchOperatorFilter[]{}, false), "field", JPASearchOperatorFilter.STARTS_WITH));

        assertThrows(InvalidFieldException.class,
                () -> ValidationUtils.searchableValidations(getSearchableInstance(new JPASearchOperatorFilter[]{},
                        new JPASearchOperatorFilter[]{}, false), "field", JPASearchOperatorFilter.STARTS_WITH));

        assertThrows(InvalidFieldException.class,
                () -> ValidationUtils.searchableValidations(getSearchableInstance(new JPASearchOperatorFilter[]{JPASearchOperatorFilter.ENDS_WITH},
                        new JPASearchOperatorFilter[]{}, false), "field", JPASearchOperatorFilter.ENDS_WITH));
    }

    @Test
    public void allowedFilterTest() {
        assertDoesNotThrow(() -> ValidationUtils.searchableValidations(getSearchableInstance(new JPASearchOperatorFilter[]{JPASearchOperatorFilter.EQ},
                new JPASearchOperatorFilter[]{JPASearchOperatorFilter.BETWEEN}, true), "field", JPASearchOperatorFilter.EQ));

        assertDoesNotThrow(() -> ValidationUtils.searchableValidations(getSearchableInstance(new JPASearchOperatorFilter[]{},
                new JPASearchOperatorFilter[]{JPASearchOperatorFilter.BETWEEN}, true), "field", JPASearchOperatorFilter.EQ));

        assertDoesNotThrow(() -> ValidationUtils.searchableValidations(getSearchableInstance(new JPASearchOperatorFilter[]{},
                new JPASearchOperatorFilter[]{}, true), "field", JPASearchOperatorFilter.EQ));

        assertDoesNotThrow(() -> ValidationUtils.searchableValidations(getSearchableInstance(new JPASearchOperatorFilter[]{},
                new JPASearchOperatorFilter[]{}, true), "field", JPASearchOperatorFilter.LT));
    }

    private static Searchable getSearchableInstance(JPASearchOperatorFilter[] allowedFilters, JPASearchOperatorFilter[] notAllowedFilters, boolean allowLike) {
        return new Searchable() {

            @Override
            public Class<? extends Annotation> annotationType() {
                return Searchable.class;
            }

            @Override
            public String entityFieldKey() {
                return "";
            }

            @Override
            public JPASearchType targetType() {
                return null;
            }

            @Override
            public String datePattern() {
                return "";
            }

            @Override
            public int maxSize() {
                return 0;
            }

            @Override
            public int minSize() {
                return 0;
            }

            @Override
            public String regexPattern() {
                return "";
            }

            @Override
            public int maxDigits() {
                return 0;
            }

            @Override
            public int minDigits() {
                return 0;
            }

            @Override
            public String decimalFormat() {
                return "";
            }

            @Override
            public boolean sortable() {
                return false;
            }

            @Override
            public boolean trim() {
                return false;
            }

            @Override
            public Tag[] tags() {
                return new Tag[0];
            }

            @Override
            public JPASearchOperatorFilter[] allowedFilters() {
                return allowedFilters;
            }

            @Override
            public JPASearchOperatorFilter[] notAllowedFilters() {
                return notAllowedFilters;
            }

            @Override
            public boolean allowLikeFilters() {
                return allowLike;
            }

            @Override
            public boolean ordinalEnum() {
                return false;
            }


        };
    }

}
