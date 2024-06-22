package app.tozzi.model;

import java.util.stream.Stream;

public enum JPASearchSortType {
    ASC, DESC;

    public static JPASearchSortType load(String sortType, JPASearchSortType defaultJPASearchSortType) {
        return Stream.of(JPASearchSortType.values()).filter(p -> p.name().equalsIgnoreCase(sortType)).findAny().orElse(defaultJPASearchSortType);
    }
}
