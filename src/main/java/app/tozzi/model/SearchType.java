package app.tozzi.model;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.math.BigDecimal;
import java.time.*;
import java.util.*;
import java.util.stream.Stream;

@AllArgsConstructor
@Getter
public enum SearchType {
    UNTYPED(Collections.emptyList()),
    STRING(List.of(String.class)),
    LONG(List.of(Long.class, long.class)),
    INTEGER(List.of(Integer.class, int.class)),
    DATE(List.of(Date.class)),
    LOCALDATE(List.of(LocalDate.class)),
    LOCALDATETIME(List.of(LocalDateTime.class)),
    LOCALTIME(List.of(LocalTime.class)),
    OFFSETDATETIME(List.of(OffsetDateTime.class)),
    OFFSETTIME(List.of(OffsetTime.class)),
    FLOAT(List.of(Float.class, float.class)),
    DOUBLE(List.of(Double.class, double.class)),
    BIGDECIMAL(List.of(BigDecimal.class)),
    BOOLEAN(List.of(Boolean.class, boolean.class));

    private final List<Class<?>> defaultClasses;

    public static SearchType load(Class<?> clazz, SearchType defaultType) {
        return Stream.of(SearchType.values()).filter(s -> s.defaultClasses.contains(clazz)).findAny().orElse(defaultType);
    }
}
