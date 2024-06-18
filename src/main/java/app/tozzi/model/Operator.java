package app.tozzi.model;

import app.tozzi.JPASearchFunctions;

import app.tozzi.utils.JPAExpressionFunction;
import app.tozzi.utils.JPAExpressionFunction2;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.stream.Stream;

@Getter
@AllArgsConstructor
public enum Operator {
    AND("and", JPASearchFunctions.AND),
    OR("or", JPASearchFunctions.OR),
    NOT("not", JPASearchFunctions.NOT),

    EQ("eq", JPASearchFunctions.EQ),
    EQ_IGNORECASE("iEq", JPASearchFunctions.EQ_IGNORECASE),
    CONTAINS("contains", JPASearchFunctions.CONTAINS),
    CONTAINS_IGNORECASE("iContains", JPASearchFunctions.CONTAINS_IGNORECASE),
    IN("in", JPASearchFunctions.IN),
    NIN("nin", JPASearchFunctions.NIN),
    STARTSWITH("startsWith", JPASearchFunctions.STARTSWITH),
    STARTSWITH_IGNORECASE("iStartsWith", JPASearchFunctions.STARTSWITH_IGNORECASE),
    ENDSWITH("endsWith", JPASearchFunctions.ENDSWITH),
    ENDSWITH_IGNORECASE("iEndsWith", JPASearchFunctions.ENDSWITH_IGNORECASE),
    NOTEQ("notEq", JPASearchFunctions.NOTEQ),
    NOTEQ_IGNORECASE("iNotEq", JPASearchFunctions.NOTEQ_IGNORECASE),
    GT("gt", JPASearchFunctions.GT),
    GTE("gte", JPASearchFunctions.GTE),
    LT("lt", JPASearchFunctions.LT),
    LTE("lte", JPASearchFunctions.LTE),
    BETWEEN("between", JPASearchFunctions.BETWEEN),

    DATE("date", JPASearchFunctions.DATE),
    ENUM("enum", JPASearchFunctions.ENUM),
    STR("str", JPASearchFunctions.STR),
    BIG_DECIMAL("bigDecimal", JPASearchFunctions.BIG_DECIMAL),

    IS_NULL("isNull", JPASearchFunctions.NULL),
    IS_EMPTY("isEmpty", JPASearchFunctions.EMPTY),
    IS_NOT_NULL("isNotNull", JPASearchFunctions.NOT_NULL),
    IS_NOT_EMPTY("isNotEmpty", JPASearchFunctions.NOT_EMPTY);

    Operator(String name, JPAExpressionFunction<?, ?> fnc) {
        this(name, fnc, null,  true);
    }
    Operator(String name, JPAExpressionFunction2<?> fnc) {
        this(name, null, fnc, false);
    }

    private final String name;
    private final JPAExpressionFunction<?, ?> function;
    private final JPAExpressionFunction2<?> function2;
    private final boolean evaluateStrings;

    public static Operator load(String name) {
        return Stream.of(Operator.values()).filter(f -> f.name.equals(name)).findAny().orElse(EQ);
    }
}
