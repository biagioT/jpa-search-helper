package app.tozzi.model;

import app.tozzi.exceptions.InvalidValueException;
import app.tozzi.utils.GenericUtils;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.*;
import java.time.format.DateTimeFormatter;
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

    public Object getValue(String field, String value, String datePattern, String decimalFormat) {

        if (value.contains(",")) {
            return Stream.of(value.split(",")).map(sv -> getValue(field, sv, datePattern, decimalFormat)).toList();
        }

        try {
            return switch (this) {
                case STRING -> value;
                case LONG -> Long.valueOf(value);
                case INTEGER -> Integer.valueOf(value);
                case DATE -> new SimpleDateFormat(datePattern).parse(value);
                case LOCALDATETIME -> LocalDateTime.parse(value, DateTimeFormatter.ofPattern(datePattern));
                case LOCALDATE -> LocalDate.parse(value, DateTimeFormatter.ofPattern(datePattern));
                case LOCALTIME -> LocalTime.parse(value, DateTimeFormatter.ofPattern(datePattern));
                case OFFSETDATETIME -> OffsetDateTime.parse(value, DateTimeFormatter.ofPattern(datePattern));
                case OFFSETTIME -> OffsetTime.parse(value, DateTimeFormatter.ofPattern(datePattern));
                case BOOLEAN -> GenericUtils.parseBoolean(value);
                case FLOAT -> {
                    Float number = Float.valueOf(value);
                    Number formatted = formatNumber(number, decimalFormat, false, field, FLOAT);
                    yield formatted.floatValue();
                }
                case DOUBLE -> {
                    Double number = Double.valueOf(value);
                    Number formatted = formatNumber(number, decimalFormat, false, field, DOUBLE);
                    yield formatted.doubleValue();
                }
                case BIGDECIMAL -> {
                    BigDecimal number = new BigDecimal(value);
                    Number formatted = formatNumber(number, decimalFormat, true, field, BIGDECIMAL);
                    yield (BigDecimal) formatted;
                }
                case UNTYPED -> throw new IllegalArgumentException();
            };

        } catch (Exception e) {
            if (e instanceof InvalidValueException ive) {
                throw ive;
            }

            throw new InvalidValueException("Unable to convert value [" + value + "] of field [" + field + "] to [" + this.name() + "] type", e, field, value);
        }
    }

    private static Number formatNumber(Number decimalNumber, String pattern, boolean bigDecimal, String field, SearchType searchType) throws ParseException {
        Number formattedNumber = GenericUtils.formatNumber(decimalNumber, pattern, bigDecimal);
        Number number = null;

        switch (searchType) {
            case INTEGER -> number = formattedNumber.intValue();
            case FLOAT -> number = formattedNumber.floatValue();
            case DOUBLE -> number = formattedNumber.doubleValue();
            case BIGDECIMAL -> number = (BigDecimal) formattedNumber;
            default -> throw new IllegalArgumentException();
        }

        if (!number.equals(decimalNumber)) {
            throw new InvalidValueException("Invalid decimal format [" + decimalNumber + "] of field [" + field + "]", field, decimalNumber);
        }

        return formattedNumber;
    }

    public int getMaxLength(Object value) {

        if (value instanceof Collection<?> coll) {
            return Collections.max(new ArrayList<>(coll).stream().map(this::getSize).toList());
        }

        return getSize(value);

    }

    public int getMinLength(Object value) {

        if (value instanceof Collection<?> coll) {
            return Collections.min(new ArrayList<>(coll).stream().map(this::getSize).toList());
        }

        return getSize(value);

    }

    public int getMaxDigits(Object value) {

        if (value instanceof Collection<?> coll) {
            return Collections.max(new ArrayList<>(coll).stream().map(this::getDigits).toList());
        }

        return getDigits(value);

    }

    public int getMinDigits(Object value) {

        if (value instanceof Collection<?> coll) {
            return Collections.min(new ArrayList<>(coll).stream().map(this::getDigits).toList());
        }

        return getDigits(value);

    }

    private int getDigits(Object value) {
        return switch (this) {
            case LONG, INTEGER, FLOAT, DOUBLE, BIGDECIMAL -> String.valueOf(value).length();
            default -> -1;
        };
    }

    private int getSize(Object value) {
        return switch (this) {
            case STRING -> String.valueOf(value).length();
            case LONG -> ((Long) value).intValue();
            case INTEGER -> (Integer) value;
            case FLOAT -> ((Float) value).intValue();
            case DOUBLE -> ((Double) value).intValue();
            case BIGDECIMAL -> ((BigDecimal) value).intValue();
            default -> -1;
        };
    }
}
