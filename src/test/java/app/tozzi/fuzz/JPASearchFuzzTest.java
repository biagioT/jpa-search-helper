package app.tozzi.fuzz;

import app.tozzi.exception.InvalidFieldException;
import app.tozzi.exception.InvalidValueException;
import app.tozzi.exception.JPASearchException;
import app.tozzi.util.GenericUtils;
import app.tozzi.util.JPASearchUtils;
import com.code_intelligence.jazzer.api.FuzzedDataProvider;
import com.code_intelligence.jazzer.junit.FuzzTest;
import org.junit.jupiter.api.Assertions;

import java.util.HashMap;

class JPASearchFuzzTest {

    @FuzzTest
    void fuzzInputParsing(FuzzedDataProvider data) {
        var filters = new HashMap<String, String>();
        var numFilters = data.consumeInt(0, 50);

        for (var i = 0; i < numFilters; i++) {
            var key = data.consumeString(100);
            var value = data.consumeString(200);
            filters.put(key, value);
        }

        var pagination = data.consumeBoolean();
        var sort = data.consumeBoolean();
        var projection = data.consumeBoolean();

        try {
            JPASearchUtils.toObject(filters, pagination, sort, projection);
        } catch (JPASearchException e) {
        } catch (Exception e) {
            Assertions.fail("Unexpected exception in input parsing", e);
        }
    }

    @FuzzTest
    void fuzzSplitter(FuzzedDataProvider data) {
        var string = data.consumeString(500);
        var separator = data.consumeString(5);
        var escape = data.consumeBoolean() ? data.consumeString(1) : null;

        try {
            GenericUtils.split(string, separator, escape);
        } catch (Exception e) {
            Assertions.fail("Unexpected exception in splitter", e);
        }
    }

    @FuzzTest
    void fuzzNumericParsing(FuzzedDataProvider data) {
        var fieldName = data.consumeString(20);
        var randomString = data.consumeString(50);

        try {
            GenericUtils.loadInt(randomString, 0);
        } catch (Exception e) {
            Assertions.fail("Unexpected exception in loadInt", e);
        }

        try {
            GenericUtils.parseInteger(fieldName, randomString);
        } catch (InvalidValueException e) {
        } catch (Exception e) {
            Assertions.fail("Unexpected exception in parseInteger", e);
        }

        try {
            GenericUtils.parseLong(fieldName, randomString);
        } catch (InvalidValueException e) {
        } catch (Exception e) {
            Assertions.fail("Unexpected exception in parseLong", e);
        }

        try {
            GenericUtils.parseFloat(fieldName, randomString);
        } catch (InvalidValueException e) {
        } catch (Exception e) {
            Assertions.fail("Unexpected exception in parseFloat", e);
        }

        try {
            GenericUtils.parseDouble(fieldName, randomString);
        } catch (InvalidValueException e) {
        } catch (Exception e) {
            Assertions.fail("Unexpected exception in parseDouble", e);
        }

        try {
            GenericUtils.parseBigDecimal(fieldName, randomString);
        } catch (InvalidValueException e) {
        } catch (Exception e) {
            Assertions.fail("Unexpected exception in parseBigDecimal", e);
        }
    }

    @FuzzTest
    void fuzzDateParsing(FuzzedDataProvider data) {
        var fieldName = data.consumeString(20);
        var randomString = data.consumeString(50);
        var pattern = data.consumeBoolean() ? "yyyy-MM-dd" : data.consumeString(20);

        try {
            GenericUtils.parseDate(fieldName, randomString, pattern);
        } catch (InvalidValueException | IllegalArgumentException e) {
        } catch (Exception e) {
            Assertions.fail("Unexpected exception in parseDate", e);
        }

        try {
            GenericUtils.parseLocalDate(fieldName, randomString, pattern);
        } catch (InvalidValueException | IllegalArgumentException e) {
        } catch (Exception e) {
            Assertions.fail("Unexpected exception in parseLocalDate", e);
        }

        try {
            GenericUtils.parseLocalDateTime(fieldName, randomString, pattern);
        } catch (InvalidValueException | IllegalArgumentException e) {
        } catch (Exception e) {
            Assertions.fail("Unexpected exception in parseLocalDateTime", e);
        }
    }

    @FuzzTest
    void fuzzBooleanParsing(FuzzedDataProvider data) {
        var fieldName = data.consumeString(20);
        var randomValue = data.consumeString(100);

        try {
            GenericUtils.parseBoolean(fieldName, randomValue);
        } catch (InvalidValueException e) {
        } catch (Exception e) {
            Assertions.fail("Unexpected exception in parseBoolean", e);
        }
    }

    @FuzzTest
    void fuzzEnumParsing(FuzzedDataProvider data) {
        var fieldName = data.consumeString(20);
        var randomValue = data.consumeString(50);
        var useOrdinal = data.consumeBoolean();

        try {
            GenericUtils.parseEnum(fieldName, randomValue, useOrdinal, TestEnum.class);
        } catch (InvalidValueException | InvalidFieldException e) {
        } catch (Exception e) {
            Assertions.fail("Unexpected exception in parseEnum", e);
        }
    }

    enum TestEnum {
        A, B, C
    }
}