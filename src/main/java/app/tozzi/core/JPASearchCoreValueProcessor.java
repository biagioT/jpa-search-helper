package app.tozzi.core;

import app.tozzi.annotation.Searchable;
import app.tozzi.exception.InvalidValueException;
import app.tozzi.exception.JPASearchException;
import app.tozzi.model.JPASearchOperatorFilter;
import app.tozzi.model.JPASearchType;
import app.tozzi.util.GenericUtils;

import java.math.BigDecimal;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

public class JPASearchCoreValueProcessor {

    protected static Object processValue(JPASearchOperatorFilter operatorFilter, JPASearchType searchType, Searchable searchable, String field, Object value, Class<?> type, boolean lower) {

        if (operatorFilter.getAllowedValues() == 0)
            return null;

        var objValue = getValue(operatorFilter, searchType, searchable, field, value, type, lower);
        if (objValue == null) {
            throw new InvalidValueException("Invalid value [" + value + "]", field, value);
        }
        searchableValidationsOnTargetValue(objValue, searchable, field, value, searchType, operatorFilter);
        filterValidations(operatorFilter, field, objValue, searchType);
        return objValue;
    }

    private static void searchableValidationsOnTargetValue(Object targetValue, Searchable searchable, String field, Object value, JPASearchType searchType, JPASearchOperatorFilter filter) {

        // Length
        var maxLength = getMaxLength(searchType, targetValue);
        if (maxLength >= 0 && searchable.maxSize() >= 0 && maxLength > searchable.maxSize()) {
            throw new InvalidValueException("Value [" + value + "] exceeds maximum length [" + searchable.maxSize() + "] defined on field [" + field + "]", field, value);
        }
        var minLength = getMinLength(searchType, targetValue);
        if (minLength >= 0 && searchable.minSize() >= 0 && minLength < searchable.minSize()) {
            throw new InvalidValueException("Value [" + value + "] less than minimum length [" + searchable.minSize() + "] defined on field [" + field + "]", field, value);
        }

        // Digits
        var maxDigits = getMaxDigits(searchType, targetValue);
        if (maxDigits >= 0 && searchable.maxDigits() >= 0 && maxDigits > searchable.maxDigits()) {
            throw new InvalidValueException("Value [" + value + "] exceeds maximum digits count [" + searchable.maxDigits() + "] defined on field [" + field + "]", field, value);
        }
        var minDigits = getMinDigits(searchType, targetValue);
        if (minLength >= 0 && searchable.minDigits() >= 0 && minDigits < searchable.minDigits()) {
            throw new InvalidValueException("Value [" + value + "] less than minimum digits count [" + searchable.minDigits() + "] defined on field [" + field + "]", field, value);
        }

        // Regex
        if (searchable.regexPattern() != null && !searchable.regexPattern().isBlank() && !matchRegex(searchType, filter, targetValue, searchable.regexPattern())) {
            throw new InvalidValueException("Value [" + value + " does not match pattern [" + searchable.regexPattern() + " of field [" + field + "]", field, value);
        }
    }

    private static void filterValidations(JPASearchOperatorFilter searchOperatorFilter, String field, Object valueObj, JPASearchType searchType) {
        var isCollection = Collection.class.isAssignableFrom(valueObj.getClass());
        var collection = isCollection ? (Collection<?>) valueObj : null;
        var values = isCollection ? collection.size() : 1;

        if (searchOperatorFilter.getAllowedValues() != -1 && searchOperatorFilter.getAllowedValues() != values) {
            throw new InvalidValueException("Invalid values count: [" + values + "] for type [" + searchType.name() + "] of field [" + field + "]. Expected: [" + searchOperatorFilter.getAllowedValues() + "]; received: [" + values + "]", field, valueObj);
        }
    }

    private static Object getValue(JPASearchOperatorFilter jpaSearchOperatorFilter, JPASearchType searchType, Searchable searchable, String field, Object value, Class<?> type, boolean lower) {

        if (value instanceof Collection<?> coll) {
            return coll.stream().map(el -> getValue(jpaSearchOperatorFilter, searchType, searchable, field, el, type, lower)).toList();
        }

        try {
            var res = switch (searchType) {
                case STRING -> String.valueOf(value);
                case DATE -> GenericUtils.parseDate(field, value, searchable.datePattern());
                case LOCALDATETIME -> GenericUtils.parseLocalDateTime(field, value, searchable.datePattern());
                case LOCALDATE -> GenericUtils.parseLocalDate(field, value, searchable.datePattern());
                case LOCALTIME -> GenericUtils.parseLocalTime(field, value, searchable.datePattern());
                case OFFSETDATETIME -> GenericUtils.parseOffsetDateTime(field, value, searchable.datePattern());
                case OFFSETTIME -> GenericUtils.parseOffsetTime(field, value, searchable.datePattern());
                case BOOLEAN -> GenericUtils.parseBoolean(field, value);
                case INTEGER, LONG, FLOAT, DOUBLE, BIGDECIMAL -> formatNumber(field, value, searchable, searchType, jpaSearchOperatorFilter);
                case ZONEDDATETIME -> GenericUtils.parseZonedDateTime(field, value, searchable.datePattern());
                case UUID -> GenericUtils.parseUUID(field, value);
                case INSTANT -> GenericUtils.parseInstant(field, value, searchable.datePattern());
                case DATE_SQL -> GenericUtils.parseSQLDate(field, value, searchable.datePattern());
                case TIME_SQL -> GenericUtils.parseSQLTime(field, value, searchable.datePattern());
                case TIMESTAMP -> GenericUtils.parseSQLTimestamp(field, value, searchable.datePattern());
                case ENUM -> GenericUtils.parseEnum(field, value, searchable.ordinalEnum(), type);
                case UNTYPED -> throw new IllegalArgumentException();
            };

            return lower ? toLowerCase(res) : res;

        } catch (Exception e) {
            if (e instanceof JPASearchException jse) {
                throw jse;
            }

            throw new InvalidValueException("Unable to convert value [" + value + "] of field [" + field + "] to [" + searchType.name() + "] type", e, field, value);
        }
    }

    private static Object formatNumber(String field, Object number, Searchable searchable, JPASearchType searchType, JPASearchOperatorFilter operatorFilter) throws ParseException {

        if (operatorFilter.isNoNumberParsing()) {
            return GenericUtils.containsOnlyDigits(number) ? number : loadNumber(field, number, searchType);
        }

        number = loadNumber(field, number, searchType);
        Number formattedNumber;

        switch (searchType) {
            case INTEGER -> formattedNumber = GenericUtils.parseInteger(field, number);
            case LONG -> formattedNumber = GenericUtils.parseLong(field, number);
            case FLOAT ->
                    formattedNumber = GenericUtils.formatNumber(loadNumber(field, number, searchType), searchable.decimalFormat(), false).floatValue();
            case DOUBLE ->
                    formattedNumber = GenericUtils.formatNumber(loadNumber(field, number, searchType), searchable.decimalFormat(), false).doubleValue();
            case BIGDECIMAL ->
                    formattedNumber = (BigDecimal) GenericUtils.formatNumber(loadNumber(field, number, searchType), searchable.decimalFormat(), true);
            default -> throw new IllegalArgumentException();
        }

        if (!number.equals(formattedNumber)) {
            throw new InvalidValueException("Invalid decimal format [" + number + "] of field [" + field + "]", field, number);
        }

        return formattedNumber;
    }

    private static Number loadNumber(String field, Object number, JPASearchType searchType) {

        return switch (searchType) {
            case INTEGER -> GenericUtils.parseInteger(field, number);
            case LONG -> GenericUtils.parseLong(field, number);
            case FLOAT -> GenericUtils.parseFloat(field, number);
            case DOUBLE -> GenericUtils.parseDouble(field, number);
            case BIGDECIMAL -> GenericUtils.parseBigDecimal(field, number);
            default -> throw new IllegalArgumentException();
        };
    }

    private static int getMaxLength(JPASearchType JPASearchType, Object value) {

        if (value instanceof Collection<?> coll) {
            return Collections.max(new ArrayList<>(coll).stream().map(e -> getSize(JPASearchType, e)).toList());
        }

        return getSize(JPASearchType, value);

    }

    private static int getMinLength(JPASearchType JPASearchType, Object value) {

        if (value instanceof Collection<?> coll) {
            return Collections.min(new ArrayList<>(coll).stream().map(e -> getSize(JPASearchType, e)).toList());
        }

        return getSize(JPASearchType, value);

    }

    private static int getMaxDigits(JPASearchType JPASearchType, Object value) {

        if (value instanceof Collection<?> coll) {
            return Collections.max(new ArrayList<>(coll).stream().map(e -> getDigits(JPASearchType, e)).toList());
        }

        return getDigits(JPASearchType, value);

    }

    private static int getMinDigits(JPASearchType JPASearchType, Object value) {

        if (value instanceof Collection<?> coll) {
            return Collections.min(new ArrayList<>(coll).stream().map(e -> getDigits(JPASearchType, e)).toList());
        }

        return getDigits(JPASearchType, value);

    }

    private static int getDigits(JPASearchType JPASearchType, Object value) {
        return switch (JPASearchType) {
            case LONG, INTEGER, FLOAT, DOUBLE, BIGDECIMAL -> String.valueOf(value).length();
            default -> -1;
        };
    }

    private static int getSize(JPASearchType JPASearchType, Object value) {
        return switch (JPASearchType) {
            case STRING -> String.valueOf(value).length();
            case LONG -> value instanceof String s ? Long.valueOf(s).intValue() : ((Long) value).intValue();
            case INTEGER -> value instanceof String s ? Integer.valueOf(s) : (Integer) value;
            case FLOAT -> ((Float) value).intValue();
            case DOUBLE -> ((Double) value).intValue();
            case BIGDECIMAL -> ((BigDecimal) value).intValue();
            default -> -1;
        };
    }

    private static boolean matchRegex(JPASearchType searchType, JPASearchOperatorFilter filter, Object value, String regex) {

        if (filter.isLike())
            return true;

        if (value instanceof Collection<?> coll) {
            return coll.stream().allMatch(v -> matchRegex(searchType, filter, v, regex));
        }

        return switch (searchType) {
            case STRING -> String.valueOf(value).matches(regex);
            default -> true;
        };

    }

    private static Object toLowerCase(Object object) {
        return object instanceof String str ? str.toLowerCase() : object;
    }


}
