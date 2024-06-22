package app.tozzi.model;

import app.tozzi.exception.JPASearchException;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.List;
import java.util.stream.Stream;

@Getter
@AllArgsConstructor
public enum JPASearchPaginationFilter {

    LIMIT("limit"), OFFSET("offset"), SORT("sort");

    private final String value;

    public static JPASearchPaginationFilter load(String value) {
        return Stream.of(JPASearchPaginationFilter.values()).filter(p -> p.getValue().equals(value)).findAny().orElseThrow(() -> new JPASearchException("Unknown pagination filter: " + value));
    }

    public static List<String> keys() {
        return Stream.of(JPASearchPaginationFilter.values()).map(JPASearchPaginationFilter::getValue).toList();
    }
}
