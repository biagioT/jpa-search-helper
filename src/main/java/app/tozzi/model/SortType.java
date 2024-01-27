package app.tozzi.model;

import java.util.stream.Stream;

public enum SortType {
    ASC, DESC;

    public static SortType load(String sortType, SortType defaultSortType) {
        return Stream.of(SortType.values()).filter(p -> p.name().equalsIgnoreCase(sortType)).findAny().orElse(defaultSortType);
    }
}
