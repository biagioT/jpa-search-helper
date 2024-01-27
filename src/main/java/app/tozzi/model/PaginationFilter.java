package app.tozzi.model;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.List;
import java.util.stream.Stream;

@Getter
@AllArgsConstructor
public enum PaginationFilter {

    LIMIT("_limit"), OFFSET("_offset"), SORT("_sort");

    private final String suffix;

    public static PaginationFilter load(String suffix) {
        return Stream.of(PaginationFilter.values()).filter(p -> p.suffix.equals(suffix)).findAny().orElse(null);
    }

    public static List<String> keys() {
        return Stream.of(PaginationFilter.values()).map(p -> p.suffix).toList();
    }
}
