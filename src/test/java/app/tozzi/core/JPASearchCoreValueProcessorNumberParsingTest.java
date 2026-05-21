package app.tozzi.core;

import app.tozzi.model.JPASearchOperatorFilter;
import app.tozzi.model.JPASearchType;
import app.tozzi.model.MyModel;
import app.tozzi.util.ReflectionUtils;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Documents the current behaviour of {@link JPASearchCoreValueProcessor} with regards to the
 * {@code noNumberParsing} flag on {@link JPASearchOperatorFilter}.
 * <p>
 * These tests are intentionally descriptive (they capture the current behaviour) so that any future
 * refactor of the flag can be done with a clear understanding of what changes for the API consumers.
 */
public class JPASearchCoreValueProcessorNumberParsingTest {

    // -- IN on INTEGER ------------------------------------------------------------------------------------------------

    @Test
    public void in_integer_digitStringValue_returnsRawString() {
        // Given a String value that contains only digits AND an operator with noNumberParsing=true (IN),
        // the value is currently returned AS-IS (String), NOT parsed to Integer.
        var searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);
        var searchable = searchableFields.get("wrapperInteger").getKey();

        var res = JPASearchCoreValueProcessor.processValue(
                JPASearchOperatorFilter.IN, JPASearchType.INTEGER, searchable, "wrapperInteger", "1234", null, false);

        assertTrue(res.isPresent());
        assertInstanceOf(String.class, res.get(), "IN on INTEGER currently keeps digit-only String values unparsed");
        assertEquals("1234", res.get());
    }

    @Test
    public void in_integer_integerValue_returnsInteger() {
        var searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);
        var searchable = searchableFields.get("wrapperInteger").getKey();

        var res = JPASearchCoreValueProcessor.processValue(
                JPASearchOperatorFilter.IN, JPASearchType.INTEGER, searchable, "wrapperInteger", 1234, null, false);

        assertTrue(res.isPresent());
        assertInstanceOf(Integer.class, res.get());
        assertEquals(1234, res.get());
    }

    @Test
    public void in_long_negativeDigitStringValue_returnsRawString() {
        // After the broadening of GenericUtils#containsOnlyDigits to accept the leading minus sign,
        // signed digit strings are now treated like positive ones: the raw value is preserved.
        var searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);
        var searchable = searchableFields.get("primitiveLong").getKey();

        var res = JPASearchCoreValueProcessor.processValue(
                JPASearchOperatorFilter.IN, JPASearchType.LONG, searchable, "primitiveLong", "-15", null, false);

        assertTrue(res.isPresent());
        assertInstanceOf(String.class, res.get(), "Signed digit strings are now recognised as 'numeric raw'");
        assertEquals("-15", res.get());
    }

    @Test
    public void in_double_decimalDigitStringValue_returnsRawString() {
        var searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);
        var searchable = searchableFields.get("wrapperDouble").getKey();

        var res = JPASearchCoreValueProcessor.processValue(
                JPASearchOperatorFilter.IN, JPASearchType.DOUBLE, searchable, "wrapperDouble", "12.500", null, false);

        assertTrue(res.isPresent());
        assertInstanceOf(String.class, res.get(),
                "Decimal digit strings are recognised as numeric raw with the broadened regex");
    }

    // -- CONTAINS / ENDS_WITH on numeric type -------------------------------------------------------------------------

    @Test
    public void contains_integer_digitString_keepsString() {
        var searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);
        var searchable = searchableFields.get("wrapperInteger").getKey();

        var res = JPASearchCoreValueProcessor.processValue(
                JPASearchOperatorFilter.CONTAINS, JPASearchType.INTEGER, searchable, "wrapperInteger", "12", null, false);

        assertTrue(res.isPresent());
        assertInstanceOf(String.class, res.get(),
                "CONTAINS on numeric column expects pattern matching → raw String is preserved");
        assertEquals("12", res.get());
    }

    @Test
    public void endsWith_integer_digitString_keepsString() {
        var searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);
        var searchable = searchableFields.get("wrapperInteger").getKey();

        var res = JPASearchCoreValueProcessor.processValue(
                JPASearchOperatorFilter.ENDS_WITH, JPASearchType.INTEGER, searchable, "wrapperInteger", "34", null, false);

        assertTrue(res.isPresent());
        assertInstanceOf(String.class, res.get());
        assertEquals("34", res.get());
    }

    // -- STARTS_WITH (noNumberParsing=false) is inconsistent w.r.t. CONTAINS / ENDS_WITH ------------------------------

    @Test
    public void startsWith_integer_digitString_isParsedToInteger() {
        // IMPORTANT: STARTS_WITH on INTEGER currently DOES parse (noNumberParsing=false), unlike
        // CONTAINS and ENDS_WITH. This is the asymmetry mentioned in the analysis.
        var searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);
        var searchable = searchableFields.get("wrapperInteger").getKey();

        var res = JPASearchCoreValueProcessor.processValue(
                JPASearchOperatorFilter.STARTS_WITH, JPASearchType.INTEGER, searchable, "wrapperInteger", "12", null, false);

        assertTrue(res.isPresent());
        assertInstanceOf(Integer.class, res.get(),
                "STARTS_WITH currently parses to Integer (inconsistent with CONTAINS/ENDS_WITH)");
        assertEquals(12, res.get());
    }

    // -- EQ (noNumberParsing=false) -----------------------------------------------------------------------------------

    @Test
    public void eq_integer_digitString_isParsedToInteger() {
        var searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);
        var searchable = searchableFields.get("wrapperInteger").getKey();

        var res = JPASearchCoreValueProcessor.processValue(
                JPASearchOperatorFilter.EQ, JPASearchType.INTEGER, searchable, "wrapperInteger", "1234", null, false);

        assertTrue(res.isPresent());
        assertInstanceOf(Integer.class, res.get());
        assertEquals(1234, res.get());
    }

    // -- Collection values --------------------------------------------------------------------------------------------

    @Test
    public void in_integer_collectionOfDigitStrings_returnsCollectionOfStrings() {
        var searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);
        var searchable = searchableFields.get("wrapperInteger").getKey();

        var res = JPASearchCoreValueProcessor.processValue(
                JPASearchOperatorFilter.IN, JPASearchType.INTEGER, searchable, "wrapperInteger",
                java.util.List.of("12", "34"), null, false);

        assertTrue(res.isPresent());
        assertInstanceOf(java.util.Collection.class, res.get());
        var coll = (java.util.Collection<?>) res.get();
        coll.forEach(e -> assertInstanceOf(String.class, e,
                "IN on INTEGER with all-digit string values keeps every element as String"));
    }

    @Test
    public void in_integer_collectionMixed_returnsMixedTypes() {
        // The recursive getValue() processes each element independently:
        // "12"  → String (matches containsOnlyDigits)
        // "-12" → String (now matches the broadened containsOnlyDigits)
        // 99    → Integer (already a Number, fallback path)
        // Mixed types in a single cb.in(...) still happen → real-world hazard to be aware of.
        var searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);
        var searchable = searchableFields.get("wrapperInteger").getKey();

        var res = JPASearchCoreValueProcessor.processValue(
                JPASearchOperatorFilter.IN, JPASearchType.INTEGER, searchable, "wrapperInteger",
                java.util.List.of("12", "-12", 99), null, false);

        assertTrue(res.isPresent());
        var coll = (java.util.Collection<?>) res.get();
        var types = coll.stream().map(Object::getClass).toList();
        assertTrue(types.contains(String.class), "all digit-string variants remain String");
        assertTrue(types.contains(Integer.class), "Number instances pass through as Integer");
    }
}

