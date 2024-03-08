package app.tozzi;

import app.tozzi.exceptions.InvalidFieldException;
import app.tozzi.exceptions.InvalidValueException;
import app.tozzi.model.SearchFilter;
import app.tozzi.test.ExampleBean;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.*;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class JPASearchCoreTests {

    @Test
    public void notSearchableTest() {
        Map<String, String> filters = new HashMap<>();
        filters.put("string_eq", "test");
        filters.put("date1", "20240125");
        InvalidFieldException ex = Assertions.assertThrows(InvalidFieldException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("string", ex.getField());
    }

    @Test
    public void notExistingFieldTest() {
        Map<String, String> filters = new HashMap<>();
        filters.put("unknowndield_eq", "test");
        filters.put("date1", "20240125");
        InvalidFieldException ex = Assertions.assertThrows(InvalidFieldException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("unknowndield", ex.getField());
    }

    @Test
    public void badTypeTest1() {
        Map<String, String> filters = new HashMap<>();
        filters.put("primitiveInteger", "string");
        filters.put("date1", "20240125");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("primitiveInteger", ex.getField());
        Assertions.assertEquals("string", ex.getValue());
    }

    @Test
    public void badTypeTest2() {
        Map<String, String> filters = new HashMap<>();
        filters.put("wrapperInteger", "string");
        filters.put("date1", "20240125");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("wrapperInteger", ex.getField());
        Assertions.assertEquals("string", ex.getValue());
    }

    @Test
    public void badTypeTest3() {
        Map<String, String> filters = new HashMap<>();
        filters.put("primitiveLong", "string");
        filters.put("date1", "20240125");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("primitiveLong", ex.getField());
        Assertions.assertEquals("string", ex.getValue());
    }

    @Test
    public void badTypeTest4() {
        Map<String, String> filters = new HashMap<>();
        filters.put("wrapperLong", "string");
        filters.put("date1", "20240125");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("wrapperLong", ex.getField());
        Assertions.assertEquals("string", ex.getValue());
    }

    @Test
    public void badTypeTest5() {
        Map<String, String> filters = new HashMap<>();
        filters.put("primitiveFloat", "string");
        filters.put("date1", "20240125");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("primitiveFloat", ex.getField());
        Assertions.assertEquals("string", ex.getValue());
    }

    @Test
    public void badTypeTest6() {
        Map<String, String> filters = new HashMap<>();
        filters.put("wrapperFloat", "string");
        filters.put("date1", "20240125");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("wrapperFloat", ex.getField());
        Assertions.assertEquals("string", ex.getValue());
    }

    @Test
    public void badTypeTest7() {
        Map<String, String> filters = new HashMap<>();
        filters.put("primitiveDouble", "string");
        filters.put("date1", "20240125");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("primitiveDouble", ex.getField());
        Assertions.assertEquals("string", ex.getValue());
    }

    @Test
    public void badTypeTest8() {
        Map<String, String> filters = new HashMap<>();
        filters.put("wrapperDouble", "string");
        filters.put("date1", "20240125");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("wrapperDouble", ex.getField());
        Assertions.assertEquals("string", ex.getValue());
    }

    @Test
    public void badSizeTest1() {
        Map<String, String> filters = new HashMap<>();
        filters.put("primitiveInteger", "11");
        filters.put("date1", "20240125");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("primitiveInteger", ex.getField());
        Assertions.assertEquals("11", ex.getValue());
    }

    @Test
    public void badSizeTest2() {
        Map<String, String> filters = new HashMap<>();
        filters.put("primitiveInteger", "4");
        filters.put("date1", "20240125");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("primitiveInteger", ex.getField());
        Assertions.assertEquals("4", ex.getValue());
    }

    @Test
    public void badLengthTest1() {
        Map<String, String> filters = new HashMap<>();
        filters.put("wrapperInteger", "1");
        filters.put("date1", "20240125");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("wrapperInteger", ex.getField());
        Assertions.assertEquals("1", ex.getValue());
    }

    @Test
    public void badLengthTest2() {
        Map<String, String> filters = new HashMap<>();
        filters.put("wrapperInteger", "12345");
        filters.put("date1", "20240125");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("wrapperInteger", ex.getField());
        Assertions.assertEquals("12345", ex.getValue());
    }

    @Test
    public void badRegexTest1() {
        Map<String, String> filters = new HashMap<>();
        filters.put("email", "biagio.tozzi");
        filters.put("date1", "20240125");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("email", ex.getField());
        Assertions.assertEquals("biagio.tozzi", ex.getValue());
    }

    @Test
    public void badRegexTest2() {
        Map<String, String> filters = new HashMap<>();
        filters.put("email_in", "biagio.tozzi@gmail.com,biagio.tozzi");
        filters.put("date1", "20240125");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("email", ex.getField());
        Assertions.assertEquals("biagio.tozzi@gmail.com,biagio.tozzi", ex.getValue());
    }

    @Test
    public void badDateTest1() {
        Map<String, String> filters = new HashMap<>();
        filters.put("date1", "25/01/2024");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("date1", ex.getField());
        Assertions.assertEquals("25/01/2024", ex.getValue());
    }

    @Test
    public void badDateTest2() {
        Map<String, String> filters = new HashMap<>();
        filters.put("date2", "25/01/2024");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("date2", ex.getField());
        Assertions.assertEquals("25/01/2024", ex.getValue());
    }

    @Test
    public void badLocalDateTimeTest() {
        Map<String, String> filters = new HashMap<>();
        filters.put("localDateTime", "25/01/2024");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("localDateTime", ex.getField());
        Assertions.assertEquals("25/01/2024", ex.getValue());
    }

    @Test
    public void badLocalDateTest() {
        Map<String, String> filters = new HashMap<>();
        filters.put("localDate", "25/01/2024");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("localDate", ex.getField());
        Assertions.assertEquals("25/01/2024", ex.getValue());
    }

    @Test
    public void badLocalTimeTest() {
        Map<String, String> filters = new HashMap<>();
        filters.put("localTime", "26:10:50");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("localTime", ex.getField());
        Assertions.assertEquals("26:10:50", ex.getValue());
    }

    @Test
    public void badOffsetDateTimeTest() {
        Map<String, String> filters = new HashMap<>();
        filters.put("offsetDateTime", "25/01/2024");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("offsetDateTime", ex.getField());
        Assertions.assertEquals("25/01/2024", ex.getValue());
    }

    @Test
    public void badOffsetTimeTest() {
        Map<String, String> filters = new HashMap<>();
        filters.put("offsetTime", "26:10:50");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("offsetTime", ex.getField());
        Assertions.assertEquals("26:10:50", ex.getValue());
    }

    @Test
    public void badDecimalFormatTest1() {
        Map<String, String> filters = new HashMap<>();
        filters.put("primitiveFloat", "1.22");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("primitiveFloat", ex.getField());
        Assertions.assertEquals(1.22f, ex.getValue());
    }

    @Test
    public void badDecimalFormatTest2() {
        Map<String, String> filters = new HashMap<>();
        filters.put("wrapperFloat", "1.234");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("wrapperFloat", ex.getField());
        Assertions.assertEquals(1.234f, ex.getValue());
    }

    @Test
    public void badDecimalFormatTest3() {
        Map<String, String> filters = new HashMap<>();
        filters.put("primitiveDouble", "1.22");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("primitiveDouble", ex.getField());
        Assertions.assertEquals(1.22, ex.getValue());
    }

    @Test
    public void badDecimalFormatTest4() {
        Map<String, String> filters = new HashMap<>();
        filters.put("wrapperDouble", "1.22");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("wrapperDouble", ex.getField());
        Assertions.assertEquals(1.22, ex.getValue());
    }

    @Test
    public void badDecimalFormatTest5() {
        Map<String, String> filters = new HashMap<>();
        filters.put("bigDecimal", "1.234");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("bigDecimal", ex.getField());
        Assertions.assertEquals(new BigDecimal("1.234"), ex.getValue());
    }

    @Test
    public void badBooleanTest1() {
        Map<String, String> filters = new HashMap<>();
        filters.put("wrapperBoolean", "88");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("wrapperBoolean", ex.getField());
        Assertions.assertEquals("88", ex.getValue());
    }

    @Test
    public void badBooleanTest2() {
        Map<String, String> filters = new HashMap<>();
        filters.put("primitiveBoolean", "88");
        InvalidValueException ex = Assertions.assertThrows(InvalidValueException.class, () -> filters.forEach((key, value) -> JPASearchCore.filterManagement(key, value, ExampleBean.class, true, null)));
        Assertions.assertEquals("primitiveBoolean", ex.getField());
        Assertions.assertEquals("88", ex.getValue());
    }

    @Test
    public void entityFieldsOverrideTest() {
        Map<String, String> filters = new HashMap<>();
        filters.put("date2", "20240125");
        Map<String, String> fields = new HashMap<>();
        fields.put("date2", "date3");
        List<JPASearchCore.FilterBean> fb = filters.entrySet().stream().map(e -> JPASearchCore.filterManagement(e.getKey(), e.getValue(), ExampleBean.class, true, fields)).toList();
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("date2") && f.getFieldKey().equals("date3")));
    }

    @Test
    public void tagTest() {
        Map<String, String> filters = new HashMap<>();
        filters.put("f1", "test1");
        filters.put("f2", "test1");
        filters.put("t.f2", "test1");
        filters.put("t.f3", "test1");
        filters.put("tf3", "test1");
        List<JPASearchCore.FilterBean> fb = filters.entrySet().stream().map(e -> JPASearchCore.filterManagement(e.getKey(), e.getValue(), ExampleBean.class, true, null)).toList();

        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("f1") && f.getFieldKey().equals("f1")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("f2") && f.getFieldKey().equals("f2")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("t.f2") && f.getFieldKey().equals("t.f2")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("t.f3") && f.getFieldKey().equals("ttt")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("tf3") && f.getFieldKey().equals("tttee")));
    }

    @Test
    public void valuesTest() {
        Map<String, String> filters = new HashMap<>();
        filters.put("primitiveInteger", "6");
        filters.put("wrapperInteger", "10");
        filters.put("email", "biagio.tozzi@gmail.com");
        filters.put("integerString", "4444444");
        filters.put("dateString", "20240125");
        filters.put("date1", "20240125");
        filters.put("date2", "20240125");
        filters.put("primitiveLong", "10");
        filters.put("wrapperLong", "10");
        filters.put("primitiveFloat", "1.3");
        filters.put("wrapperFloat", "1.30");
        filters.put("primitiveDouble", "1.3");
        filters.put("wrapperDouble", "1.3");
        filters.put("bigDecimal", "1.35");
        filters.put("localDateTime", "2024-01-25T00:00:00");
        filters.put("localDate", "2024-01-25");
        filters.put("localTime", "101158Z");
        filters.put("offsetDateTime", "2024-01-25T00:00:00Z");
        filters.put("offsetTime", "101158Z");
        filters.put("nestedBean.string", "Nested!");
        filters.put("primitiveBoolean", "true");
        filters.put("wrapperBoolean", "false");

        List<JPASearchCore.FilterBean> fb = filters.entrySet().stream().map(e -> JPASearchCore.filterManagement(e.getKey(), e.getValue(), ExampleBean.class, true, null)).toList();

        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("primitiveInteger")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("wrapperInteger")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("email")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("integerString")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("dateString")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("date1")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("date2")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("primitiveLong")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("wrapperLong")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("primitiveFloat")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("wrapperFloat")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("primitiveDouble")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("bigDecimal")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("localDateTime")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("localDate")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("localTime")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("offsetDateTime")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("offsetTime")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("nestedBean.string")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("primitiveBoolean")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("wrapperBoolean")));

        Calendar calendar = Calendar.getInstance();
        calendar.set(2024, Calendar.JANUARY, 25, 0, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);

        Assertions.assertEquals(6, fb.stream().filter(f -> f.getOriginalKey().equals("primitiveInteger")).findAny().orElseThrow().getValue());
        Assertions.assertEquals(10, fb.stream().filter(f -> f.getOriginalKey().equals("wrapperInteger")).findAny().orElseThrow().getValue());
        Assertions.assertEquals("biagio.tozzi@gmail.com", fb.stream().filter(f -> f.getOriginalKey().equals("email")).findAny().orElseThrow().getValue());
        Assertions.assertEquals(4444444, fb.stream().filter(f -> f.getOriginalKey().equals("integerString")).findAny().orElseThrow().getValue());
        Assertions.assertEquals(calendar.getTime(), fb.stream().filter(f -> f.getOriginalKey().equals("dateString")).findAny().orElseThrow().getValue());
        Assertions.assertEquals(calendar.getTime(), fb.stream().filter(f -> f.getOriginalKey().equals("date1")).findAny().orElseThrow().getValue());
        Assertions.assertEquals(calendar.getTime(), fb.stream().filter(f -> f.getOriginalKey().equals("date2")).findAny().orElseThrow().getValue());
        Assertions.assertEquals(10L, fb.stream().filter(f -> f.getFieldKey().equals("entity.long-one")).findAny().orElseThrow().getValue());
        Assertions.assertEquals(10L, fb.stream().filter(f -> f.getFieldKey().equals("entity.long-two")).findAny().orElseThrow().getValue());
        Assertions.assertEquals(1.3f, fb.stream().filter(f -> f.getOriginalKey().equals("primitiveFloat")).findAny().orElseThrow().getValue());
        Assertions.assertEquals(1.30f, fb.stream().filter(f -> f.getOriginalKey().equals("wrapperFloat")).findAny().orElseThrow().getValue());
        Assertions.assertEquals(1.3d, fb.stream().filter(f -> f.getOriginalKey().equals("wrapperDouble")).findAny().orElseThrow().getValue());
        Assertions.assertEquals(1.3d, fb.stream().filter(f -> f.getOriginalKey().equals("primitiveDouble")).findAny().orElseThrow().getValue());
        Assertions.assertEquals(new BigDecimal("1.35"), fb.stream().filter(f -> f.getOriginalKey().equals("bigDecimal")).findAny().orElseThrow().getValue());
        Assertions.assertEquals(LocalDateTime.of(2024, Month.JANUARY, 25, 0, 0, 0, 0), fb.stream().filter(f -> f.getOriginalKey().equals("localDateTime")).findAny().orElseThrow().getValue());
        Assertions.assertEquals(LocalDate.of(2024, Month.JANUARY, 25), fb.stream().filter(f -> f.getOriginalKey().equals("localDate")).findAny().orElseThrow().getValue());
        Assertions.assertEquals(LocalTime.of(10, 11, 58), fb.stream().filter(f -> f.getOriginalKey().equals("localTime")).findAny().orElseThrow().getValue());
        Assertions.assertEquals(OffsetDateTime.of(LocalDateTime.of(2024, Month.JANUARY, 25, 0, 0, 0, 0), ZoneOffset.of("Z")), fb.stream().filter(f -> f.getOriginalKey().equals("offsetDateTime")).findAny().orElseThrow().getValue());
        Assertions.assertEquals(OffsetTime.of(LocalTime.of(10, 11, 58), ZoneOffset.of("Z")), fb.stream().filter(f -> f.getOriginalKey().equals("offsetTime")).findAny().orElseThrow().getValue());
        Assertions.assertEquals("Nested!", fb.stream().filter(f -> f.getOriginalKey().equals("nestedBean.string")).findAny().orElseThrow().getValue());
        Assertions.assertEquals(true, fb.stream().filter(f -> f.getOriginalKey().equals("primitiveBoolean")).findAny().orElseThrow().getValue());
        Assertions.assertEquals(false, fb.stream().filter(f -> f.getOriginalKey().equals("wrapperBoolean")).findAny().orElseThrow().getValue());

    }

    @Test
    public void  filtersTest() {
        Map<String, String> filters = new HashMap<>();
        filters.put("primitiveInteger_eq", "6");
        filters.put("wrapperInteger_is", "null");
        filters.put("email_iEq", "biagio.tozzi@gmail.com");
        filters.put("integerString_is", "empty");
        filters.put("dateString_is", "not_null");
        filters.put("date1_gt", "20240125");
        filters.put("date2_gte", "20240125");
        filters.put("primitiveLong_lt", "10");
        filters.put("wrapperLong_lte", "10");
        filters.put("primitiveFloat_between", "1.3,1.4");
        filters.put("wrapperFloat_is", "not_empty");
        filters.put("primitiveDouble_in", "1.3,1.4");
        filters.put("wrapperDouble_nin", "1.3,1.4");
        filters.put("bigDecimal_notEq", "1.35");
        filters.put("nestedBean.string_contains", "Nested!");
        filters.put("nestedBean.string2_iContains", "Nested!");
        filters.put("nestedBean.string3_startsWith", "Nested!");
        filters.put("nestedBean.string4_iStartsWith", "Nested!");
        filters.put("nestedBean.string5_endsWith", "Nested!");
        filters.put("nestedBean.string6_iEndsWith", "Nested!");
        filters.put("nestedBean.string7_iNotEq", "Nested!");

        List<JPASearchCore.FilterBean> fb = filters.entrySet().stream().map(e -> JPASearchCore.filterManagement(e.getKey(), e.getValue(), ExampleBean.class, true, null)).toList();

        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("primitiveInteger")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("wrapperInteger")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("email")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("integerString")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("dateString")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("date1")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("date2")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("primitiveLong")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("wrapperLong")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("primitiveFloat")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("wrapperFloat")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("primitiveDouble")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("wrapperDouble")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("bigDecimal")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("nestedBean.string")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("nestedBean.string2")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("nestedBean.string3")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("nestedBean.string4")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("nestedBean.string5")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("nestedBean.string6")));
        Assertions.assertTrue(fb.stream().anyMatch(f -> f.getOriginalKey().equals("nestedBean.string7")));

        Assertions.assertEquals(SearchFilter.EQ, fb.stream().filter(f -> f.getOriginalKey().equals("primitiveInteger")).findAny().orElseThrow().getSearchFilter());
        Assertions.assertEquals(SearchFilter.EQ_IGNORECASE, fb.stream().filter(f -> f.getOriginalKey().equals("email")).findAny().orElseThrow().getSearchFilter());
        Assertions.assertEquals(SearchFilter.IS_EMPTY, fb.stream().filter(f -> f.getOriginalKey().equals("integerString")).findAny().orElseThrow().getSearchFilter());
        Assertions.assertEquals(SearchFilter.IS_NULL, fb.stream().filter(f -> f.getOriginalKey().equals("wrapperInteger")).findAny().orElseThrow().getSearchFilter());
        Assertions.assertEquals(SearchFilter.IS_NOT_NULL, fb.stream().filter(f -> f.getOriginalKey().equals("dateString")).findAny().orElseThrow().getSearchFilter());
        Assertions.assertEquals(SearchFilter.GT, fb.stream().filter(f -> f.getOriginalKey().equals("date1")).findAny().orElseThrow().getSearchFilter());
        Assertions.assertEquals(SearchFilter.GTE, fb.stream().filter(f -> f.getOriginalKey().equals("date2")).findAny().orElseThrow().getSearchFilter());
        Assertions.assertEquals(SearchFilter.LT, fb.stream().filter(f -> f.getFieldKey().equals("entity.long-one")).findAny().orElseThrow().getSearchFilter());
        Assertions.assertEquals(SearchFilter.LTE, fb.stream().filter(f -> f.getFieldKey().equals("entity.long-two")).findAny().orElseThrow().getSearchFilter());
        Assertions.assertEquals(SearchFilter.BETWEEN, fb.stream().filter(f -> f.getOriginalKey().equals("primitiveFloat")).findAny().orElseThrow().getSearchFilter());
        Assertions.assertEquals(SearchFilter.IS_NOT_EMPTY, fb.stream().filter(f -> f.getOriginalKey().equals("wrapperFloat")).findAny().orElseThrow().getSearchFilter());
        Assertions.assertEquals(SearchFilter.NIN, fb.stream().filter(f -> f.getOriginalKey().equals("wrapperDouble")).findAny().orElseThrow().getSearchFilter());
        Assertions.assertEquals(SearchFilter.IN, fb.stream().filter(f -> f.getOriginalKey().equals("primitiveDouble")).findAny().orElseThrow().getSearchFilter());
        Assertions.assertEquals(SearchFilter.CONTAINS, fb.stream().filter(f -> f.getOriginalKey().equals("nestedBean.string")).findAny().orElseThrow().getSearchFilter());
        Assertions.assertEquals(SearchFilter.CONTAINS_IGNORECASE, fb.stream().filter(f -> f.getOriginalKey().equals("nestedBean.string2")).findAny().orElseThrow().getSearchFilter());
        Assertions.assertEquals(SearchFilter.STARTSWITH, fb.stream().filter(f -> f.getOriginalKey().equals("nestedBean.string3")).findAny().orElseThrow().getSearchFilter());
        Assertions.assertEquals(SearchFilter.STARTSWITH_IGNORECASE, fb.stream().filter(f -> f.getOriginalKey().equals("nestedBean.string4")).findAny().orElseThrow().getSearchFilter());
        Assertions.assertEquals(SearchFilter.ENDSWITH, fb.stream().filter(f -> f.getOriginalKey().equals("nestedBean.string5")).findAny().orElseThrow().getSearchFilter());
        Assertions.assertEquals(SearchFilter.ENDSWITH_IGNORECASE, fb.stream().filter(f -> f.getOriginalKey().equals("nestedBean.string6")).findAny().orElseThrow().getSearchFilter());
        Assertions.assertEquals(SearchFilter.NOTEQ_IGNORECASE, fb.stream().filter(f -> f.getOriginalKey().equals("nestedBean.string7")).findAny().orElseThrow().getSearchFilter());
    }

}
