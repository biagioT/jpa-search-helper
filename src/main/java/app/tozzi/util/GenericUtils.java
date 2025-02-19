package app.tozzi.util;

import app.tozzi.exception.InvalidValueException;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.regex.Pattern;

public class GenericUtils {

    public static boolean containsSeparator(String string, String separator, String escapeSeparatorChar) {
        return escapeSeparatorChar != null ? Pattern.compile("(?<!" + Pattern.quote(escapeSeparatorChar) + ")" + Pattern.quote(separator)).matcher(string).find() : string.contains(separator);
    }

    public static boolean containsSeparatorWithEscape(String string, String separator, String escapeSeparatorChar) {
        return string.contains(escapeSeparatorChar + separator);
    }

    public static Integer loadInt(String value, int defaultInt) {
        try {
            return Integer.parseInt(value);

        } catch (Exception e) {
            return defaultInt;
        }
    }

    public static List<String> split(String string, String separator, String escapeSeparatorChar) {

        List<String> result = new ArrayList<>();
        if (string == null || separator == null) {
            return result;
        }

        var regex = escapeSeparatorChar != null ? "(?<!" + Pattern.quote(escapeSeparatorChar) + ")" + Pattern.quote(separator) : separator;
        var tokens = string.split(regex);
        for (var token : tokens) {
            result.add(token.replace(escapeSeparatorChar + separator, separator));
        }

        if (string.endsWith(separator) && !string.endsWith(escapeSeparatorChar + separator)) {
            result.add("");
        }

        return result;
    }

    public static Number formatNumber(Number decimalNumber, String pattern, boolean bigDecimal) throws ParseException {
        var df = new DecimalFormat(pattern);
        df.setParseBigDecimal(bigDecimal);
        return df.parse(df.format(decimalNumber));
    }

    public static Float parseFloat(String field, Object value) {

        if (value instanceof Float fl) {
            return fl;
        }

        if (value instanceof String fl) {
            return Float.parseFloat(fl);
        }

        throw new InvalidValueException("Invalid float number [" + value + "]", field, value);
    }

    public static Long parseLong(String field, Object value) {

        if (value instanceof Long ln) {
            return ln;
        }

        if (value instanceof String ln) {
            return Long.parseLong(ln);
        }

        throw new InvalidValueException("Invalid long number [" + value + "]", field, value);
    }

    public static Integer parseInteger(String field, Object value) {

        if (value instanceof Integer in) {
            return in;
        }

        if (value instanceof String in) {
            return Integer.parseInt(in);
        }

        throw new InvalidValueException("Invalid number [" + value + "]", field, value);
    }

    public static Double parseDouble(String field, Object value) {

        if (value instanceof Double db) {
            return db;
        }

        if (value instanceof String db) {
            return Double.parseDouble(db);
        }

        throw new InvalidValueException("Invalid double number [" + value + "]", field, value);
    }

    public static BigDecimal parseBigDecimal(String field, Object value) {

        if (value instanceof BigDecimal db) {
            return db;
        }

        if (value instanceof String db) {
            return new BigDecimal(db);
        }

        throw new InvalidValueException("Invalid big decimal number [" + value + "]", field, value);
    }

    public static boolean containsOnlyDigits(Object number) {
        return number instanceof String str && str.matches("\\d+");
    }

    public static boolean parseBoolean(String field, Object value) {

        if (value instanceof Boolean bool) {
            return bool;
        }

        if ("true".equalsIgnoreCase(value.toString())) {
            return true;
        }

        if ("false".equalsIgnoreCase(value.toString())) {
            return false;
        }

        throw new InvalidValueException("Invalid boolean value [" + value + "]", field, value);
    }

    public static Date parseDate(String field, Object value, String pattern) throws ParseException {

        if (value instanceof Date d) {
            return d;
        }

        if (value instanceof String str) {
            return new SimpleDateFormat(pattern).parse(str);
        }

        throw new InvalidValueException("Invalid date value [" + value + "]", field, value);
    }

    public static LocalDate parseLocalDate(String field, Object value, String pattern) {

        if (value instanceof LocalDate ld) {
            return ld;
        }

        if (value instanceof String str) {
            return LocalDate.parse(str, DateTimeFormatter.ofPattern(pattern));
        }

        throw new InvalidValueException("Invalid local date value [" + value + "]", field, value);
    }

    public static LocalTime parseLocalTime(String field, Object value, String pattern) {

        if (value instanceof LocalTime lt) {
            return lt;
        }

        if (value instanceof String str) {
            return LocalTime.parse(str, DateTimeFormatter.ofPattern(pattern));
        }

        throw new InvalidValueException("Invalid local time [" + value + "]", field, value);
    }

    public static LocalDateTime parseLocalDateTime(String field, Object value, String pattern) {

        if (value instanceof LocalDateTime ldt) {
            return ldt;
        }

        if (value instanceof String str) {
            return LocalDateTime.parse(str, DateTimeFormatter.ofPattern(pattern));
        }

        throw new InvalidValueException("Invalid local date time value [" + value + "]", field, value);
    }

    public static OffsetTime parseOffsetTime(String field, Object value, String pattern) {

        if (value instanceof OffsetTime ot) {
            return ot;
        }

        if (value instanceof String str) {
            return OffsetTime.parse(str, DateTimeFormatter.ofPattern(pattern));
        }

        throw new InvalidValueException("Invalid offset time value [" + value + "]", field, value);
    }

    public static OffsetDateTime parseOffsetDateTime(String field, Object value, String pattern) {

        if (value instanceof OffsetDateTime odt) {
            return odt;
        }

        if (value instanceof String str) {
            return OffsetDateTime.parse(str, DateTimeFormatter.ofPattern(pattern));
        }

        throw new InvalidValueException("Invalid offset date time value [" + value + "]", field, value);
    }

    public static ZonedDateTime parseZonedDateTime(String field, Object value, String pattern) {

        if (value instanceof ZonedDateTime zdt) {
            return zdt;
        }

        if (value instanceof String str) {
            return ZonedDateTime.parse(str, DateTimeFormatter.ofPattern(pattern));
        }

        throw new InvalidValueException("Invalid zoned date time value [" + value + "]", field, value);
    }

    public static UUID parseUUID(String field, Object value) {

        if (value instanceof UUID uuid) {
            return uuid;
        }

        if (value instanceof String str) {
            return UUID.fromString(str);
        }

        throw new InvalidValueException("Invalid UUID value [" + value + "]", field, value);
    }

}
