package app.tozzi.model;

import app.tozzi.JPASearchFunctions;
import javax.persistence.criteria.Predicate;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NonNull;

import java.util.function.Function;
import java.util.stream.Stream;

@Getter
@AllArgsConstructor
public enum SearchFilter {

    EQ("eq", JPASearchFunctions.EQ, false, null, 1, false, false),
    EQ_IGNORECASE("iEq", JPASearchFunctions.EQ_IGNORECASE, false, null, 1, false, false),
    CONTAINS("contains", JPASearchFunctions.CONTAINS, false, null, 1, true, true),
    CONTAINS_IGNORECASE("iContains", JPASearchFunctions.CONTAINS_IGNORECASE, false, null, 1, true, true),
    IN("in", JPASearchFunctions.IN, false, null, -1, false, false),
    NIN("nin", JPASearchFunctions.NIN, false, null, -1, false, false),
    STARTSWITH("startsWith", JPASearchFunctions.STARTSWITH, false, null, 1, false, true),
    STARTSWITH_IGNORECASE("iStartsWith", JPASearchFunctions.STARTSWITH_IGNORECASE, false, null, 1, false, true),
    ENDSWITH("endsWith", JPASearchFunctions.ENDSWITH, false, null, 1, true, true),
    ENDSWITH_IGNORECASE("iEndsWith", JPASearchFunctions.ENDSWITH_IGNORECASE, false, null, 1, true, true),
    NOTEQ("notEq", JPASearchFunctions.NOTEQ, false, null, 1, false, false),
    NOTEQ_IGNORECASE("iNotEq", JPASearchFunctions.NOTEQ_IGNORECASE, false, null, 1, false, false),
    GT("gt", JPASearchFunctions.GT, true, null, 1, false, false),
    GTE("gte", JPASearchFunctions.GTE, true, null, 1, false, false),
    LT("lt", JPASearchFunctions.LT, true, null, 1, false, false),
    LTE("lte", JPASearchFunctions.LTE, true, null, 1, false, false),
    BETWEEN("between", JPASearchFunctions.BETWEEN, true, null, 2, false, false),
    IS_NULL("is", JPASearchFunctions.NULL, false, "null", 1, false, false),
    IS_EMPTY("is", JPASearchFunctions.EMPTY, false, "empty", 1, false, false),
    IS_NOT_NULL("is", JPASearchFunctions.NOT_NULL, false, "not_null", 1, false, false),
    IS_NOT_EMPTY("is", JPASearchFunctions.NOT_EMPTY, false, "not_empty", 1, false, false);

    private final String name;
    private final Function<FieldRootBuilderBean<?>, Predicate> function;
    private final boolean comparable;
    private final String fixedValue;
    private final int allowedValues;
    private final boolean noNumberParsing;
    private final boolean like;

    public static SearchFilter load(String suffix, String fixedValue, @NonNull SearchFilter defaultSearchFilter) {
        return Stream.of(SearchFilter.values()).filter(f -> f.name.equals(suffix) && (f.fixedValue == null || fixedValue.equalsIgnoreCase(f.fixedValue))).findAny().orElse(defaultSearchFilter);
    }

    public boolean hasFixedValue() {
        return this.fixedValue != null && !this.fixedValue.isBlank();
    }

}
