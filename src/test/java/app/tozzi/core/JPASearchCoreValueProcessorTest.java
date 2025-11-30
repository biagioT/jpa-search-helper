package app.tozzi.core;

import app.tozzi.exception.InvalidValueException;
import app.tozzi.model.JPASearchOperatorFilter;
import app.tozzi.model.JPASearchType;
import app.tozzi.model.MyEnum;
import app.tozzi.model.MyModel;
import app.tozzi.util.ReflectionUtils;
import org.junit.jupiter.api.Test;

import java.util.Calendar;
import java.util.Date;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

public class JPASearchCoreValueProcessorTest {

    @Test
    public void processValue() {

        var searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);

        var res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("id").getKey(), "id", "12345", null, false);
        assertTrue(res.isPresent());
        assertEquals(12345L, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("id").getKey(), "id", 12345L, null, false);
        assertTrue(res.isPresent());
        assertEquals(12345L, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.UUID, searchableFields.get("uuid").getKey(), "uuid", "7409d9e4-ee9d-11ef-9ff1-83157c916723", null, false);
        assertTrue(res.isPresent());
        assertEquals(UUID.fromString("7409d9e4-ee9d-11ef-9ff1-83157c916723"), res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.STRING, searchableFields.get("stringOne").getKey(), "stringOne", "test", null, false);
        assertTrue(res.isPresent());
        assertEquals("test", res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.STRING, searchableFields.get("stringOne").getKey(), "stringOne", "TEST", null, true);
        assertTrue(res.isPresent());
        assertEquals("test", res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.STRING, searchableFields.get("stringMail").getKey(), "mail", "biagio.tozzi@gmail.com", null, true);
        assertTrue(res.isPresent());
        assertEquals("biagio.tozzi@gmail.com", res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.INTEGER, searchableFields.get("primitiveInteger").getKey(), "primitiveInteger", 9, null, false);
        assertTrue(res.isPresent());
        assertEquals(9, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.INTEGER, searchableFields.get("primitiveInteger").getKey(), "primitiveInteger", "9", null, false);
        assertTrue(res.isPresent());
        assertEquals(9, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.INTEGER, searchableFields.get("wrapperInteger").getKey(), "wrapperInteger", 9999, null, false);
        assertTrue(res.isPresent());
        assertEquals(9999, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.INTEGER, searchableFields.get("wrapperInteger").getKey(), "wrapperInteger", "9999", null, false);
        assertTrue(res.isPresent());
        assertEquals(9999, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DATE, searchableFields.get("stringDate").getKey(), "stringDate", "20240622", null, false);
        assertTrue(res.isPresent());
        var cal = Calendar.getInstance();
        cal.setTime((Date) res.get());
        assertEquals(2024, cal.get(Calendar.YEAR));
        assertEquals(Calendar.JUNE, cal.get(Calendar.MONTH));
        assertEquals(22, cal.get(Calendar.DAY_OF_MONTH));

        var now = new Date();
        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DATE, searchableFields.get("dateOne").getKey(), "dateOne", now, null, false);
        assertTrue(res.isPresent());
        assertEquals(now, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("primitiveLong").getKey(), "primitiveLong", 9999L, null, false);
        assertTrue(res.isPresent());
        assertEquals(9999L, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("primitiveLong").getKey(), "primitiveLong", "9999", null, false);
        assertTrue(res.isPresent());
        assertEquals(9999L, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("wrapperLong").getKey(), "wrapperLong.one", 9999L, null, false);
        assertTrue(res.isPresent());
        assertEquals(9999L, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("wrapperLong").getKey(), "wrapperLong.one", "9999", null, false);
        assertTrue(res.isPresent());
        assertEquals(9999L, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("wrapperLong").getKey(), "wrapperLong.one", 9999L, null, false);
        assertTrue(res.isPresent());
        assertEquals(9999L, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("wrapperLong").getKey(), "wrapperLongYes", "9999", null, false);
        assertTrue(res.isPresent());
        assertEquals(9999L, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("primitiveLong").getKey(), "wrapperLongYes", 9999L, null, false);
        assertTrue(res.isPresent());
        assertEquals(9999L, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.FLOAT, searchableFields.get("primitiveFloat").getKey(), "primitiveFloat", "23.54", null, false);
        assertTrue(res.isPresent());
        assertEquals(23.54f, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.FLOAT, searchableFields.get("primitiveFloat").getKey(), "primitiveFloat", 23.54f, null, false);
        assertTrue(res.isPresent());
        assertEquals(23.54f, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.FLOAT, searchableFields.get("wrapperFloat").getKey(), "wrapperFloat", "23.54", null, false);
        assertTrue(res.isPresent());
        assertEquals(23.54f, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.FLOAT, searchableFields.get("wrapperFloat").getKey(), "wrapperFloat", 23.54f, null, false);
        assertTrue(res.isPresent());
        assertEquals(23.54f, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.DOUBLE, searchableFields.get("primitiveDouble").getKey(), "primitiveDoubleYes", "23.54", null, false);
        assertTrue(res.isPresent());
        assertEquals(23.54d, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.DOUBLE, searchableFields.get("primitiveDouble").getKey(), "primitiveDoubleYes", 23.54d, null, false);
        assertTrue(res.isPresent());
        assertEquals(23.54d, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.DOUBLE, searchableFields.get("wrapperDouble").getKey(), "wrapperDouble", "23.544", null, false);
        assertTrue(res.isPresent());
        assertEquals(23.544d, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.DOUBLE, searchableFields.get("wrapperDouble").getKey(), "wrapperDouble", 23.544d, null, false);
        assertTrue(res.isPresent());
        assertEquals(23.544d, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.DOUBLE, searchableFields.get("wrapperDouble").getKey(), "wrapperDouble", 23.54d, null, false);
        assertTrue(res.isPresent());
        assertEquals(23.540d, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.ENUM, searchableFields.get("myEnum").getKey(), "myEnum", "EN_VAL_1", MyEnum.class, false);
        assertTrue(res.isPresent());
        assertEquals(MyEnum.EN_VAL_1, res.get());

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.ENUM, searchableFields.get("myEnum2").getKey(), "myEnum2", "1", MyEnum.class, false);
        assertTrue(res.isPresent());
        assertEquals(MyEnum.EN_VAL_2, res.get());

        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.STRING, searchableFields.get("stringMail").getKey(), "email", "biagio.tozzi#gmail.com", null, true));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.INTEGER, searchableFields.get("primitiveInteger").getKey(), "primitiveInteger", "11", null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.INTEGER, searchableFields.get("primitiveInteger").getKey(), "primitiveInteger", "a", null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.INTEGER, searchableFields.get("wrapperInteger").getKey(), "wrapperInteger", "11111", null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.INTEGER, searchableFields.get("wrapperInteger").getKey(), "wrapperInteger", "a", null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("wrapperLong").getKey(), "wrapperLong", "a", null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("wrapperLong").getKey(), "wrapperLong", 1, null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("primitiveLong").getKey(), "primitiveLong", "a", null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("primitiveLong").getKey(), "primitiveLong", 1, null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DATE, searchableFields.get("stringDate").getKey(), "stringDate", "22/06/2024", null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DATE, searchableFields.get("stringDate").getKey(), "stringDate", "", null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DATE, searchableFields.get("dateOne").getKey(), "dateOne", "22/06/2024", null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DATE, searchableFields.get("dateOne").getKey(), "dateOne", "", null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.FLOAT, searchableFields.get("primitiveFloat").getKey(), "primitiveFloat", "58.8998", null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.FLOAT, searchableFields.get("primitiveFloat").getKey(), "primitiveFloat", "test", null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.FLOAT, searchableFields.get("primitiveFloat").getKey(), "primitiveFloat", 58.9899f, null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.FLOAT, searchableFields.get("wrapperFloat").getKey(), "wrapperFloat", "test", null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.FLOAT, searchableFields.get("wrapperFloat").getKey(), "wrapperFloat", 58.9899f, null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.FLOAT, searchableFields.get("wrapperFloat").getKey(), "wrapperFloat", "58.8998", null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DOUBLE, searchableFields.get("primitiveDouble").getKey(), "primitiveDoubleYes", "test", null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DOUBLE, searchableFields.get("primitiveDouble").getKey(), "primitiveDoubleYes", 58.9899d, null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DOUBLE, searchableFields.get("primitiveDouble").getKey(), "primitiveDoubleYes", "58.8998", null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DOUBLE, searchableFields.get("wrapperDouble").getKey(), "wrapperDouble", "test", null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DOUBLE, searchableFields.get("wrapperDouble").getKey(), "wrapperDouble", 58.9899d, null, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DOUBLE, searchableFields.get("wrapperDouble").getKey(), "wrapperDouble", "58.8998", null, false));
    }
}