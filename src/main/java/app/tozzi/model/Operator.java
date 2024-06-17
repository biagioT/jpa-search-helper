package app.tozzi.model;

import app.tozzi.JPASearchFunctions;

import app.tozzi.utils.JPAExpressionFunction;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.stream.Stream;

@Getter
@AllArgsConstructor
public enum Operator {
    AND("and", JPASearchFunctions.AND, false,  0, false, false),
    OR("or", JPASearchFunctions.OR, false,  0, false, false),
    NOT("not", JPASearchFunctions.NOT, false,  0, false, false),

    EQ("eq", JPASearchFunctions.EQ, false, 1, false, false),
    EQ_IGNORECASE("iEq", JPASearchFunctions.EQ_IGNORECASE, false,  1, false, false),
    CONTAINS("contains", JPASearchFunctions.CONTAINS, false,  1, true, true),
    CONTAINS_IGNORECASE("iContains", JPASearchFunctions.CONTAINS_IGNORECASE, false,  1, true, true),
    IN("in", JPASearchFunctions.IN, false,  -1, false, false),
    NIN("nin", JPASearchFunctions.NIN, false,  -1, false, false),
    STARTSWITH("startsWith", JPASearchFunctions.STARTSWITH, false,  1, false, true),
    STARTSWITH_IGNORECASE("iStartsWith", JPASearchFunctions.STARTSWITH_IGNORECASE, false,  1, false, true),
    ENDSWITH("endsWith", JPASearchFunctions.ENDSWITH, false,  1, true, true),
    ENDSWITH_IGNORECASE("iEndsWith", JPASearchFunctions.ENDSWITH_IGNORECASE, false,  1, true, true),
    NOTEQ("notEq", JPASearchFunctions.NOTEQ, false,  1, false, false),
    NOTEQ_IGNORECASE("iNotEq", JPASearchFunctions.NOTEQ_IGNORECASE, false,  1, false, false),
    GT("gt", JPASearchFunctions.GT, true,  1, false, false),
    GTE("gte", JPASearchFunctions.GTE, true,  1, false, false),
    LT("lt", JPASearchFunctions.LT, true,  1, false, false),
    LTE("lte", JPASearchFunctions.LTE, true,  1, false, false),
    BETWEEN("between", JPASearchFunctions.BETWEEN, true,  2, false, false),

    DATE("date", JPASearchFunctions.DATE, true,  2, false, false),
    BIG_DECIMAL("bigDecimal", JPASearchFunctions.BIG_DECIMAL, true,  2, false, false),



    IS_NULL("isNull", JPASearchFunctions.NULL, false,  1, false, false),
    IS_EMPTY("isEmpty", JPASearchFunctions.EMPTY, false,  1, false, false),
    IS_NOT_NULL("isNotNull", JPASearchFunctions.NOT_NULL, false,  1, false, false),
    IS_NOT_EMPTY("isNotEmpty", JPASearchFunctions.NOT_EMPTY, false,  1, false, false);

    private final String name;
    private final JPAExpressionFunction<?, ?> function;
    private final boolean comparable;
    private final int allowedValues;
    private final boolean noNumberParsing;
    private final boolean like;

    public static Operator load(String name) {
        return Stream.of(Operator.values()).filter(f -> f.name.equals(name)).findAny().orElse(EQ);
    }
}
