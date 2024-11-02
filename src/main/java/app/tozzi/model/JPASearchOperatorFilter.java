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

    EQ("eq", JPASearchFunctions.EQ, false, 1, false),
    CONTAINS("contains", JPASearchFunctions.CONTAINS, true, 1, true),
    IN("in", JPASearchFunctions.IN, true, -1, false),
    STARTS_WITH("startsWith", JPASearchFunctions.STARTSWITH, false, 1, true),
    ENDS_WITH("endsWith", JPASearchFunctions.ENDSWITH, true, 1, true),
    GT("gt", JPASearchFunctions.GT, false, 1, false),
    GTE("gte", JPASearchFunctions.GTE, false, 1, false),
    LT("lt", JPASearchFunctions.LT, false, 1, false),
    LTE("lte", JPASearchFunctions.LTE, false, 1, false),
    BETWEEN("between", JPASearchFunctions.BETWEEN, false, 2, false),
    EMPTY("empty", JPASearchFunctions.EMPTY, false, 0, false),
    NULL("null", JPASearchFunctions.NULL, false, 0, false);

    private final String value;
    private final JPASearchFunction<?, ?> function;
    private final boolean noNumberParsing;
    private final int allowedValues;
    private final boolean like;

    public static JPASearchOperatorFilter load(String name) {
        return Stream.of(JPASearchOperatorFilter.values()).filter(f -> f.getValue().equals(name)).findAny()
                .orElseThrow(() -> new JPASearchException("Unknown operator: " + name));
    }

    public static List<String> getAllValues() {
        return Stream.of(JPASearchOperatorFilter.values()).map(v -> v.value).toList();
    }

}
