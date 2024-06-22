package app.tozzi.model;

import app.tozzi.exception.JPASearchException;
import app.tozzi.function.JPASearchFunctions;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.List;
import java.util.stream.Stream;

@Getter
@AllArgsConstructor
public enum JPASearchOperatorFilter {

    EQ("eq", JPASearchFunctions.EQ, false, 1, false, null),
    CONTAINS("contains", JPASearchFunctions.CONTAINS, true, 1, true, null),
    IN("in", JPASearchFunctions.IN, true, -1, false, null),
    STARTS_WITH("startsWith", JPASearchFunctions.STARTSWITH, false, 1, true, null),
    ENDS_WITH("endsWith", JPASearchFunctions.ENDSWITH, false, 1, true, null),
    GT("gt", JPASearchFunctions.GT, false, 1, false, null),
    GTE("gte", JPASearchFunctions.GTE, false, 1, false, null),
    LT("lt", JPASearchFunctions.LT, false, 1, false, null),
    LTE("lte", JPASearchFunctions.LTE, false, 1, false, null),
    BETWEEN("between", JPASearchFunctions.BETWEEN, false, 2, false, null),
    EMPTY("empty", JPASearchFunctions.EMPTY, false, 0, false, List.of("true", "false", true, false)),
    NULL("null", JPASearchFunctions.NULL, false, 0, false, List.of("true", "false", true, false));

    private final String value;
    private final JPASearchFunction<?, ?> function;
    private final boolean noNumberParsing;
    private final int allowedValues;
    private final boolean like;
    private final List<Object> fixedValues;

    public static JPASearchOperatorFilter load(String name) {
        return Stream.of(JPASearchOperatorFilter.values()).filter(f -> f.getValue().equals(name)).findAny()
                .orElseThrow(() -> new JPASearchException("Unknown operator: " + name));
    }

}
