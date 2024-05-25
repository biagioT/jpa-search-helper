package app.tozzi.model;

import app.tozzi.JPASearchFunctions;
import jakarta.persistence.criteria.Predicate;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NonNull;

import java.util.function.Function;
import java.util.stream.Stream;

@Getter
@AllArgsConstructor
public enum SearchFilter {

    EQ("_eq", JPASearchFunctions.EQ, false, null, 1, false),
    EQ_IGNORECASE("_iEq", JPASearchFunctions.EQ_IGNORECASE, false, null, 1, false),
    CONTAINS("_contains", JPASearchFunctions.CONTAINS, false, null, 1, true),
    CONTAINS_IGNORECASE("_iContains", JPASearchFunctions.CONTAINS_IGNORECASE, false, null, 1, true),
    IN("_in", JPASearchFunctions.IN, false, null, -1, false),
    NIN("_nin", JPASearchFunctions.NIN, false, null, -1, false),
    STARTSWITH("_startsWith", JPASearchFunctions.STARTSWITH, false, null, 1, false),
    STARTSWITH_IGNORECASE("_iStartsWith", JPASearchFunctions.STARTSWITH_IGNORECASE, false, null, 1, false),
    ENDSWITH("_endsWith", JPASearchFunctions.ENDSWITH, false, null, 1, true),
    ENDSWITH_IGNORECASE("_iEndsWith", JPASearchFunctions.ENDSWITH_IGNORECASE, false, null, 1, true),
    NOTEQ("_notEq", JPASearchFunctions.NOTEQ, false, null, 1, false),
    NOTEQ_IGNORECASE("_iNotEq", JPASearchFunctions.NOTEQ_IGNORECASE, false, null, 1, false),
    GT("_gt", JPASearchFunctions.GT, true, null, 1, false),
    GTE("_gte", JPASearchFunctions.GTE, true, null, 1, false),
    LT("_lt", JPASearchFunctions.LT, true, null, 1, false),
    LTE("_lte", JPASearchFunctions.LTE, true, null, 1, false),
    BETWEEN("_between", JPASearchFunctions.BETWEEN, true, null, 2, false),
    IS_NULL("_is", JPASearchFunctions.NULL, false, "null", 1, false),
    IS_EMPTY("_is", JPASearchFunctions.EMPTY, false, "empty", 1, false),
    IS_NOT_NULL("_is", JPASearchFunctions.NOT_NULL, false, "not_null", 1, false),
    IS_NOT_EMPTY("_is", JPASearchFunctions.NOT_EMPTY, false, "not_empty", 1, false);

    private final String suffix;
    private final Function<FieldRootBuilderBean<?>, Predicate> function;
    private final boolean comparable;
    private final String fixedValue;
    private final int allowedValues;
    private final boolean noNumberParsing;

    public static SearchFilter load(String suffix, String fixedValue, @NonNull SearchFilter defaultSearchFilter) {
        return Stream.of(SearchFilter.values()).filter(f -> f.suffix.equals(suffix) && (f.fixedValue == null || fixedValue.equalsIgnoreCase(f.fixedValue))).findAny().orElse(defaultSearchFilter);
    }

    public boolean hasFixedValue() {
        return this.fixedValue != null && !this.fixedValue.isBlank();
    }

}
