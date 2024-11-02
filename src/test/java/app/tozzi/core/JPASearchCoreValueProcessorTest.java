package app.tozzi.core;

import app.tozzi.exception.InvalidValueException;
import app.tozzi.model.JPASearchOperatorFilter;
import app.tozzi.model.JPASearchType;
import app.tozzi.model.MyModel;
import app.tozzi.util.ReflectionUtils;
import org.junit.jupiter.api.Test;

import java.util.Calendar;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.*;

public class JPASearchCoreValueProcessorTest {

    @Test
    public void processValue() {

        var searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);
        var res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("id").getKey(), "id", "12345", false);
        assertNotNull(res);
        assertEquals(12345L, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("id").getKey(), "id", 12345L, false);
        assertNotNull(res);
        assertEquals(12345L, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.STRING, searchableFields.get("stringOne").getKey(), "stringOne", "test", false);
        assertNotNull(res);
        assertEquals("test", res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.STRING, searchableFields.get("stringOne").getKey(), "stringOne", "TEST", true);
        assertNotNull(res);
        assertEquals("test", res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.STRING, searchableFields.get("stringMail").getKey(), "mail", "biagio.tozzi@gmail.com", true);
        assertNotNull(res);
        assertEquals("biagio.tozzi@gmail.com", res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.INTEGER, searchableFields.get("primitiveInteger").getKey(), "primitiveInteger", 9, false);
        assertNotNull(res);
        assertEquals(9, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.INTEGER, searchableFields.get("primitiveInteger").getKey(), "primitiveInteger", "9", false);
        assertNotNull(res);
        assertEquals(9, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.INTEGER, searchableFields.get("wrapperInteger").getKey(), "wrapperInteger", 9999, false);
        assertNotNull(res);
        assertEquals(9999, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.INTEGER, searchableFields.get("wrapperInteger").getKey(), "wrapperInteger", "9999", false);
        assertNotNull(res);
        assertEquals(9999, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DATE, searchableFields.get("stringDate").getKey(), "stringDate", "20240622", false);
        assertNotNull(res);
        var cal = Calendar.getInstance();
        cal.setTime((Date) res);
        assertEquals(2024, cal.get(Calendar.YEAR));
        assertEquals(Calendar.JUNE, cal.get(Calendar.MONTH));
        assertEquals(22, cal.get(Calendar.DAY_OF_MONTH));

        var now = new Date();
        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DATE, searchableFields.get("dateOne").getKey(), "dateOne", now, false);
        assertNotNull(res);
        assertEquals(now, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("primitiveLong").getKey(), "primitiveLong", 9999L, false);
        assertNotNull(res);
        assertEquals(9999L, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("primitiveLong").getKey(), "primitiveLong", "9999", false);
        assertNotNull(res);
        assertEquals(9999L, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("wrapperLong").getKey(), "wrapperLong.one", 9999L, false);
        assertNotNull(res);
        assertEquals(9999L, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("wrapperLong").getKey(), "wrapperLong.one", "9999", false);
        assertNotNull(res);
        assertEquals(9999L, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("wrapperLong").getKey(), "wrapperLong.one", 9999L, false);
        assertNotNull(res);
        assertEquals(9999L, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("wrapperLong").getKey(), "wrapperLongYes", "9999", false);
        assertNotNull(res);
        assertEquals(9999L, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("primitiveLong").getKey(), "wrapperLongYes", 9999L, false);
        assertNotNull(res);
        assertEquals(9999L, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.FLOAT, searchableFields.get("primitiveFloat").getKey(), "primitiveFloat", "23.54", false);
        assertNotNull(res);
        assertEquals(23.54f, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.FLOAT, searchableFields.get("primitiveFloat").getKey(), "primitiveFloat", 23.54f, false);
        assertNotNull(res);
        assertEquals(23.54f, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.FLOAT, searchableFields.get("wrapperFloat").getKey(), "wrapperFloat", "23.54", false);
        assertNotNull(res);
        assertEquals(23.54f, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.FLOAT, searchableFields.get("wrapperFloat").getKey(), "wrapperFloat", 23.54f, false);
        assertNotNull(res);
        assertEquals(23.54f, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.DOUBLE, searchableFields.get("primitiveDouble").getKey(), "primitiveDoubleYes", "23.54", false);
        assertNotNull(res);
        assertEquals(23.54d, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.DOUBLE, searchableFields.get("primitiveDouble").getKey(), "primitiveDoubleYes", 23.54d, false);
        assertNotNull(res);
        assertEquals(23.54d, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.DOUBLE, searchableFields.get("wrapperDouble").getKey(), "wrapperDouble", "23.544", false);
        assertNotNull(res);
        assertEquals(23.544d, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.DOUBLE, searchableFields.get("wrapperDouble").getKey(), "wrapperDouble", 23.544d, false);
        assertNotNull(res);
        assertEquals(23.544d, res);

        res = JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.DOUBLE, searchableFields.get("wrapperDouble").getKey(), "wrapperDouble", 23.54d, false);
        assertNotNull(res);
        assertEquals(23.540d, res);

        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.STRING, searchableFields.get("stringMail").getKey(), "email", "biagio.tozzi#gmail.com", true));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.INTEGER, searchableFields.get("primitiveInteger").getKey(), "primitiveInteger", "11", false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.INTEGER, searchableFields.get("primitiveInteger").getKey(), "primitiveInteger", "a", false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.INTEGER, searchableFields.get("wrapperInteger").getKey(), "wrapperInteger", "11111", false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.INTEGER, searchableFields.get("wrapperInteger").getKey(), "wrapperInteger", "a", false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("wrapperLong").getKey(), "wrapperLong", "a", false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("wrapperLong").getKey(), "wrapperLong", 1, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("primitiveLong").getKey(), "primitiveLong", "a", false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.EQ, JPASearchType.LONG, searchableFields.get("primitiveLong").getKey(), "primitiveLong", 1, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DATE, searchableFields.get("stringDate").getKey(), "stringDate", "22/06/2024", false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DATE, searchableFields.get("stringDate").getKey(), "stringDate", "", false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DATE, searchableFields.get("dateOne").getKey(), "dateOne", "22/06/2024", false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DATE, searchableFields.get("dateOne").getKey(), "dateOne", "", false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.FLOAT, searchableFields.get("primitiveFloat").getKey(), "primitiveFloat", "58.8998", false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.FLOAT, searchableFields.get("primitiveFloat").getKey(), "primitiveFloat", "test", false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.FLOAT, searchableFields.get("primitiveFloat").getKey(), "primitiveFloat", 58.9899f, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.FLOAT, searchableFields.get("wrapperFloat").getKey(), "wrapperFloat", "test", false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.FLOAT, searchableFields.get("wrapperFloat").getKey(), "wrapperFloat", 58.9899f, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.FLOAT, searchableFields.get("wrapperFloat").getKey(), "wrapperFloat", "58.8998", false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DOUBLE, searchableFields.get("primitiveDouble").getKey(), "primitiveDoubleYes", "test", false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DOUBLE, searchableFields.get("primitiveDouble").getKey(), "primitiveDoubleYes", 58.9899d, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DOUBLE, searchableFields.get("primitiveDouble").getKey(), "primitiveDoubleYes", "58.8998", false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DOUBLE, searchableFields.get("wrapperDouble").getKey(), "wrapperDouble", "test", false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DOUBLE, searchableFields.get("wrapperDouble").getKey(), "wrapperDouble", 58.9899d, false));
        assertThrows(InvalidValueException.class, () -> JPASearchCoreValueProcessor.processValue(JPASearchOperatorFilter.GT, JPASearchType.DOUBLE, searchableFields.get("wrapperDouble").getKey(), "wrapperDouble", "58.8998", false));

    }

}
