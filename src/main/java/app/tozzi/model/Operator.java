package app.tozzi.model;

import app.tozzi.JPASearchFunctions;

import app.tozzi.exceptions.JPASearchException;
import app.tozzi.utils.JPAFuncWithExpressions;
import app.tozzi.utils.JPAFuncWithObjects;
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
    CONTAINS("contains", JPASearchFunctions.CONTAINS),
    IN("in", JPASearchFunctions.IN),
    STARTSWITH("startsWith", JPASearchFunctions.STARTSWITH),
    ENDSWITH("endsWith", JPASearchFunctions.ENDSWITH),
    GT("gt", JPASearchFunctions.GT),
    GTE("gte", JPASearchFunctions.GTE),
    LT("lt", JPASearchFunctions.LT),
    LTE("lte", JPASearchFunctions.LTE),
    BETWEEN("between", JPASearchFunctions.BETWEEN),

    LOWER("lower", JPASearchFunctions.LOWER),

    DATE("date", JPASearchFunctions.DATE),
    ENUM("enum", JPASearchFunctions.ENUM),
    FIELD("field", JPASearchFunctions.FIELD),
    BIG_DECIMAL("bigDecimal", JPASearchFunctions.BIG_DECIMAL),
    PERIOD("period", JPASearchFunctions.PERIOD),

    IS_NULL("isNull", JPASearchFunctions.NULL),
    IS_EMPTY("isEmpty", JPASearchFunctions.EMPTY);

    Operator(String name, JPAFuncWithExpressions<?, ?> fnc) {
        this(name, fnc, null,  true);
    }
    Operator(String name, JPAFuncWithObjects<?> fnc) {
        this(name, null, fnc, false);
    }

    private final String name;
    private final JPAFuncWithExpressions<?, ?> exprFunction;
    private final JPAFuncWithObjects<?> objFunction;
    private final boolean evaluateStrings;

    public static Operator load(String name) {
        return Stream.of(Operator.values()).filter(f -> f.name.equals(name)).findAny().orElseThrow(() -> new JPASearchException("Unknown operator: " + name));
    }
}
