package com.gisgro.model;

import com.gisgro.JPASearchFunctions;
import com.gisgro.exceptions.JPASearchException;
import com.gisgro.utils.JPAFuncWithExpressions;
import com.gisgro.utils.JPAFuncWithObjects;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.HashMap;

@Getter
@AllArgsConstructor
public class Operator {
    public Operator(String name, JPAFuncWithExpressions<?, ?> fnc) {
        this(name, fnc, null, true);
    }

    public Operator(String name, JPAFuncWithObjects<?> fnc) {
        this(name, null, fnc, false);
    }

    private final String name;
    private final JPAFuncWithExpressions<?, ?> exprFunction;
    private final JPAFuncWithObjects<?> objFunction;
    private final boolean evaluateStrings;

    private static HashMap<String, Operator> operators = null;

    private static void initializeOperators() {
        if (operators != null) {
            return; // Already initialized
        }
        operators = new HashMap<>();
        Operator[] operatorArray = {
                new Operator("and", JPASearchFunctions.AND),
                new Operator("or", JPASearchFunctions.OR),
                new Operator("not", JPASearchFunctions.NOT),
                new Operator("eq", JPASearchFunctions.EQ),
                new Operator("contains", JPASearchFunctions.CONTAINS),
                new Operator("in", JPASearchFunctions.IN),
                new Operator("startsWith", JPASearchFunctions.STARTSWITH),
                new Operator("endsWith", JPASearchFunctions.ENDSWITH),
                new Operator("gt", JPASearchFunctions.GT),
                new Operator("gte", JPASearchFunctions.GTE),
                new Operator("lt", JPASearchFunctions.LT),
                new Operator("lte", JPASearchFunctions.LTE),
                new Operator("between", JPASearchFunctions.BETWEEN),
                new Operator("lower", JPASearchFunctions.LOWER),
                new Operator("date", JPASearchFunctions.DATE),
                new Operator("enum", JPASearchFunctions.ENUM),
                new Operator("field", JPASearchFunctions.FIELD),
                new Operator("bigDecimal", JPASearchFunctions.BIG_DECIMAL),
                new Operator("period", JPASearchFunctions.PERIOD),
                new Operator("isNull", JPASearchFunctions.NULL),
                new Operator("isEmpty", JPASearchFunctions.EMPTY)
        };
        for (Operator operator : operatorArray) {
            operators.put(operator.getName(), operator);
        }
    }

    public static void addOperator(Operator operator) {
        if (operators == null) {
            initializeOperators();
        }
        operators.put(operator.getName(), operator);
    }

    public static Operator load(String name) {
        if (operators == null) {
            initializeOperators();
        }
        var operator = operators.get(name);
        if (operator == null) {
            throw new JPASearchException("Unknown operator: " + name);
        }
        return operator;
    }
}
