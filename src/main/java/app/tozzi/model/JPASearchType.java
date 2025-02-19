package app.tozzi.model;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.math.BigDecimal;
import java.time.*;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.stream.Stream;

@AllArgsConstructor
@Getter
public enum JPASearchType {

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
    ZONEDDATETIME(List.of(ZonedDateTime.class)),
    FLOAT(List.of(Float.class, float.class)),
    DOUBLE(List.of(Double.class, double.class)),
    BIGDECIMAL(List.of(BigDecimal.class)),
    BOOLEAN(List.of(Boolean.class, boolean.class)),
    UUID(List.of(java.util.UUID.class));

    private final List<Class<?>> defaultClasses;

    public static JPASearchType load(Class<?> clazz, JPASearchType defaultType) {
        return Stream.of(JPASearchType.values()).filter(s -> s.defaultClasses.contains(clazz)).findAny().orElse(defaultType);
    }
}
