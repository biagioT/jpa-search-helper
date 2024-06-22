package app.tozzi.util;

import app.tozzi.exception.InvalidValueException;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.text.ParseException;
import java.time.*;
import java.time.format.DateTimeParseException;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class GenericUtilsTest {

    @Test
    public void containsSeparatorTest() {
        assertTrue(GenericUtils.containsSeparator("test1,test2", ",", null));
        assertTrue(GenericUtils.containsSeparator("test1,test2", ",", "\\"));
        assertTrue(GenericUtils.containsSeparator("test1,test2\\,test3", ",", "\\"));
        assertTrue(GenericUtils.containsSeparator("test1,test2|,test3", ",", "|"));
    }

    @Test
    public void notContainsSeparatorTest() {
        assertFalse(GenericUtils.containsSeparator("test1,test2", ";", null));
        assertFalse(GenericUtils.containsSeparator("test1\\,test2", ",", "\\"));
        assertFalse(GenericUtils.containsSeparator("test1;,test2", ",", ";"));
    }

    @Test
    public void loadIntTest() {
        assertEquals(10, GenericUtils.loadInt("10", 1));
        assertEquals(1, GenericUtils.loadInt("10a", 1));
        assertEquals(10, GenericUtils.loadInt( String.valueOf(Long.MAX_VALUE), 10));
    }

    @Test
    public void splitTest() {
        List<String> splitted = GenericUtils.split("test1,test2", ",", null);
        assertNotNull(splitted);
        assertFalse(splitted.isEmpty());
        assertEquals(2, splitted.size());
        assertEquals("test1", splitted.getFirst());
        assertEquals("test2", splitted.getLast());

        splitted = GenericUtils.split("test1,test2\\,test3", ",", "\\");
        assertNotNull(splitted);
        assertFalse(splitted.isEmpty());
        assertEquals(2, splitted.size());
        assertEquals("test1", splitted.getFirst());
        assertEquals("test2,test3", splitted.getLast());

        splitted = GenericUtils.split("test1,test2|,test3", ",", "|");
        assertNotNull(splitted);
        assertFalse(splitted.isEmpty());
        assertEquals(2, splitted.size());
        assertEquals("test1", splitted.getFirst());
        assertEquals("test2,test3", splitted.getLast());

        splitted = GenericUtils.split("test1,test2", ";", null);
        assertNotNull(splitted);
        assertFalse(splitted.isEmpty());
        assertEquals(1, splitted.size());
        assertEquals("test1,test2", splitted.getFirst());

        splitted = GenericUtils.split("test1\\,test2", ",", "\\");
        assertNotNull(splitted);
        assertFalse(splitted.isEmpty());
        assertEquals(1, splitted.size());
        assertEquals("test1,test2", splitted.getFirst());

        splitted = GenericUtils.split("test1;,test2", ",", ";");
        assertNotNull(splitted);
        assertFalse(splitted.isEmpty());
        assertEquals(1, splitted.size());
        assertEquals("test1,test2", splitted.getFirst());

        splitted = GenericUtils.split("test1,test2,", ",", null);
        assertNotNull(splitted);
        assertFalse(splitted.isEmpty());
        assertEquals(3, splitted.size());
        assertEquals("test1", splitted.getFirst());
        assertEquals("test2", splitted.get(1));
        assertEquals("", splitted.getLast());

        splitted = GenericUtils.split("test1\\,test2,test3,", ",", "\\");
        assertNotNull(splitted);
        assertFalse(splitted.isEmpty());
        assertEquals(3, splitted.size());
        assertEquals("test1,test2", splitted.getFirst());
        assertEquals("test3", splitted.get(1));
        assertEquals("", splitted.getLast());
    }

    @Test
    public void formatNumberTest() throws ParseException {
        Number res = GenericUtils.formatNumber(10.0f, "#.000", false);
        assertNotNull(res);
        assertEquals(10.000f, res.floatValue(), 0.0001);

        res = GenericUtils.formatNumber(10.549f, "#.##", false);
        assertNotNull(res);
        assertEquals(10.55f, res.floatValue());

        res = GenericUtils.formatNumber(10.0d, "#.000", false);
        assertNotNull(res);
        assertEquals(10.000d, res.doubleValue(), 0.0001);

        res = GenericUtils.formatNumber(10.549d, "#.##", false);
        assertNotNull(res);
        assertEquals(10.55d, res.doubleValue());

        res = GenericUtils.formatNumber(new BigDecimal("10.0"), "#.000", true);
        assertNotNull(res);
        assertEquals(new BigDecimal("10.000"), res);

        res = GenericUtils.formatNumber(new BigDecimal("10.549"), "#.##", true);
        assertNotNull(res);
        assertEquals(new BigDecimal("10.55"), res);
    }

    @Test
    public void parseFloatTest() {
        Float fl = GenericUtils.parseFloat("field", 10f);
        assertEquals(10f, fl);

        fl = GenericUtils.parseFloat("field", "10");
        assertEquals(10f, fl);

        assertThrows(InvalidValueException.class, () -> GenericUtils.parseFloat("field", 10));
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseFloat("field", 10L));
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseFloat("field", 10d));
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseFloat("field", Collections.emptyList()));
    }

    @Test
    public void parseLongTest() {
        Long ln = GenericUtils.parseLong("field", 10L);
        assertEquals(10L, ln);

        ln = GenericUtils.parseLong("field", "10");
        assertEquals(10L, ln);

        assertThrows(InvalidValueException.class, () -> GenericUtils.parseLong("field", 10));
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseLong("field", 10f));
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseLong("field", 10d));
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseLong("field", Collections.emptyList()));
    }

    @Test
    public void parseDoubleTest() {
        Double db = GenericUtils.parseDouble("field", 10d);
        assertEquals(10d, db);

        db = GenericUtils.parseDouble("field", "10");
        assertEquals(10d, db);

        assertThrows(InvalidValueException.class, () -> GenericUtils.parseDouble("field", 10));
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseDouble("field", 10f));
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseDouble("field", 10L));
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseDouble("field", Collections.emptyList()));
    }

    @Test
    public void parseIntegerTest() {
        Integer i = GenericUtils.parseInteger("field", 10);
        assertEquals(10, i);

        i = GenericUtils.parseInteger("field", "10");
        assertEquals(10, i);

        assertThrows(InvalidValueException.class, () -> GenericUtils.parseInteger("field", 10d));
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseInteger("field", 10f));
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseInteger("field", 10L));
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseInteger("field", Collections.emptyList()));
    }

    @Test
    public void parseBigDecimalTest() {
        BigDecimal bd = GenericUtils.parseBigDecimal("field", new BigDecimal(10));
        assertEquals(new BigDecimal(10), bd);

        bd = GenericUtils.parseBigDecimal("field", "10");
        assertEquals(new BigDecimal(10), bd);

        assertThrows(InvalidValueException.class, () -> GenericUtils.parseBigDecimal("field", 10d));
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseBigDecimal("field", 10f));
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseBigDecimal("field", 10L));
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseBigDecimal("field", Collections.emptyList()));
    }

    @Test
    public void containsOnlyDigitsTest() {
        assertTrue(GenericUtils.containsOnlyDigits("01234"));
        assertTrue(GenericUtils.containsOnlyDigits("1"));
        assertTrue(GenericUtils.containsOnlyDigits("1234"));
        assertFalse(GenericUtils.containsOnlyDigits(1234));
        assertFalse(GenericUtils.containsOnlyDigits("test"));
        assertFalse(GenericUtils.containsOnlyDigits("123t"));
    }

    @Test
    public void parseBooleanTest() {
        assertTrue(GenericUtils.parseBoolean("test", true));
        assertTrue(GenericUtils.parseBoolean("test", "true"));
        assertTrue(GenericUtils.parseBoolean("test", Boolean.TRUE));
        assertFalse(GenericUtils.parseBoolean("test", false));
        assertFalse(GenericUtils.parseBoolean("test", Boolean.FALSE));
        assertFalse(GenericUtils.parseBoolean("test", "false"));
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseBoolean("test", 0));
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseBoolean("test", 1));
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseBoolean("test", Collections.emptyList()));
    }

    @Test
    public void parseDateTest() throws ParseException {
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseDate("field", 20240622, "yyyy-MM-dd"));
        assertThrows(ParseException.class, () -> GenericUtils.parseDate("field", "22/06/2024", "yyyyMMdd"));

        var now = new Date();
        assertEquals(now, GenericUtils.parseDate("field", now, null));

        Date date = GenericUtils.parseDate("field", "2024-06-22", "yyyy-MM-dd");
        assertNotNull(date);
        Calendar cal = Calendar.getInstance();
        cal.setTime(date);
        assertEquals(Calendar.JUNE, cal.get(Calendar.MONTH));
        assertEquals(2024, cal.get(Calendar.YEAR));
        assertEquals(22, cal.get(Calendar.DAY_OF_MONTH));

        date = GenericUtils.parseDate("field", "20240622", "yyyyMMdd");
        assertNotNull(date);
        cal = Calendar.getInstance();
        cal.setTime(date);
        assertEquals(Calendar.JUNE, cal.get(Calendar.MONTH));
        assertEquals(2024, cal.get(Calendar.YEAR));
        assertEquals(22, cal.get(Calendar.DAY_OF_MONTH));

        date = GenericUtils.parseDate("field", "2024-06-22 13:55:01", "yyyy-MM-dd HH:mm:ss");
        assertNotNull(date);
        cal = Calendar.getInstance();
        cal.setTime(date);
        assertEquals(Calendar.JUNE, cal.get(Calendar.MONTH));
        assertEquals(2024, cal.get(Calendar.YEAR));
        assertEquals(22, cal.get(Calendar.DAY_OF_MONTH));
        assertEquals(13, cal.get(Calendar.HOUR_OF_DAY));
        assertEquals(55, cal.get(Calendar.MINUTE));
        assertEquals(1, cal.get(Calendar.SECOND));

        date = GenericUtils.parseDate("field", "2024-06-22 09:00:00.234", "yyyy-MM-dd HH:mm:ss.SSS");
        assertNotNull(date);
        cal = Calendar.getInstance();
        cal.setTime(date);
        assertEquals(Calendar.JUNE, cal.get(Calendar.MONTH));
        assertEquals(2024, cal.get(Calendar.YEAR));
        assertEquals(22, cal.get(Calendar.DAY_OF_MONTH));
        assertEquals(9, cal.get(Calendar.HOUR_OF_DAY));
        assertEquals(0, cal.get(Calendar.MINUTE));
        assertEquals(0, cal.get(Calendar.SECOND));
        assertEquals(234, cal.get(Calendar.MILLISECOND));
    }

    @Test
    public void parseLocalDateTest() {
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseLocalDate("field", 20240622, "yyyy-MM-dd"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseLocalDate("field", "22/06/2024", "yyyy-MM-dd"));

        var now = LocalDate.now();
        assertEquals(now, GenericUtils.parseLocalDate("field", now, null));

        LocalDate date = GenericUtils.parseLocalDate("field", "2024-06-22", "yyyy-MM-dd");
        assertNotNull(date);
        assertEquals(Month.JUNE, date.getMonth());
        assertEquals(2024, date.getYear());
        assertEquals(22, date.getDayOfMonth());

        date = GenericUtils.parseLocalDate("field", "20240622", "yyyyMMdd");
        assertNotNull(date);
        assertNotNull(date);
        assertEquals(Month.JUNE, date.getMonth());
        assertEquals(2024, date.getYear());
        assertEquals(22, date.getDayOfMonth());

        date = GenericUtils.parseLocalDate("field", "2024-06-22 13:55:01", "yyyy-MM-dd HH:mm:ss");
        assertNotNull(date);
        assertEquals(Month.JUNE, date.getMonth());
        assertEquals(2024, date.getYear());
        assertEquals(22, date.getDayOfMonth());

        date = GenericUtils.parseLocalDate("field", "2024-06-22 09:00:00.234", "yyyy-MM-dd HH:mm:ss.SSS");
        assertNotNull(date);
        assertEquals(Month.JUNE, date.getMonth());
        assertEquals(2024, date.getYear());
        assertEquals(22, date.getDayOfMonth());
    }

    @Test
    public void parseLocalDateTimeTest() {
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseLocalDateTime("field", 20240622152200L, "yyyy-MM-dd"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseLocalDateTime("field", "22/06/2024 15:01:02", "yyyy-MM-dd"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseLocalDateTime("field", "2024-06-22", "yyyy-MM-dd"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseLocalDateTime("field", "20240622", "yyyyMMdd"));

        var now = LocalDateTime.now();
        assertEquals(now, GenericUtils.parseLocalDateTime("field", now, null));

        LocalDateTime date = GenericUtils.parseLocalDateTime("field", "2024-06-22 13:55:01", "yyyy-MM-dd HH:mm:ss");
        assertNotNull(date);
        assertEquals(Month.JUNE, date.getMonth());
        assertEquals(2024, date.getYear());
        assertEquals(22, date.getDayOfMonth());
        assertEquals(13, date.getHour());
        assertEquals(55, date.getMinute());
        assertEquals(1, date.getSecond());

        date = GenericUtils.parseLocalDateTime("field", "2024-06-22 09:00:00.234", "yyyy-MM-dd HH:mm:ss.SSS");
        assertNotNull(date);
        assertEquals(Month.JUNE, date.getMonth());
        assertEquals(2024, date.getYear());
        assertEquals(22, date.getDayOfMonth());
        assertEquals(9, date.getHour());
        assertEquals(0, date.getMinute());
        assertEquals(0, date.getSecond());
        assertEquals(234000000, date.getNano());
    }

    @Test
    public void parseOffsetDateTime() {
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseOffsetDateTime("field", 20240622152200L, "yyyy-MM-dd"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseOffsetDateTime("field", "22/06/2024 15:01:02", "yyyy-MM-dd"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseOffsetDateTime("field", "2024-06-22", "yyyy-MM-dd"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseOffsetDateTime("field", "20240622", "yyyyMMdd"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseOffsetDateTime("field", "2024-06-22 09:00:00.234", "yyyy-MM-dd HH:mm:ss.SSS"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseOffsetDateTime("field", "2024-06-22 13:55:01", "yyyy-MM-dd HH:mm:ss"));

        var now = OffsetDateTime.now();
        assertEquals(now, GenericUtils.parseOffsetDateTime("field", now, null));

        OffsetDateTime date = GenericUtils.parseOffsetDateTime("field", "2024-06-22T13:55:01+02:00", "yyyy-MM-dd'T'HH:mm:ssXXXXX");
        assertNotNull(date);
        assertEquals(Month.JUNE, date.getMonth());
        assertEquals(2024, date.getYear());
        assertEquals(22, date.getDayOfMonth());
        assertEquals(13, date.getHour());
        assertEquals(55, date.getMinute());
        assertEquals(1, date.getSecond());
        assertEquals(ZoneOffset.of("+02:00"), date.getOffset());

        date = GenericUtils.parseOffsetDateTime("field", "2024-06-22T09:00:00.234+01:00", "yyyy-MM-dd'T'HH:mm:ss.SSSXXXXX");
        assertNotNull(date);
        assertEquals(Month.JUNE, date.getMonth());
        assertEquals(2024, date.getYear());
        assertEquals(22, date.getDayOfMonth());
        assertEquals(9, date.getHour());
        assertEquals(0, date.getMinute());
        assertEquals(0, date.getSecond());
        assertEquals(234000000, date.getNano());
        assertEquals(ZoneOffset.of("+01:00"), date.getOffset());
    }

    @Test
    public void parseZonedDateTime() {
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseZonedDateTime("field", 20240622152200L, "yyyy-MM-dd"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseZonedDateTime("field", "22/06/2024 15:01:02", "yyyy-MM-dd"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseZonedDateTime("field", "2024-06-22", "yyyy-MM-dd"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseZonedDateTime("field", "20240622", "yyyyMMdd"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseZonedDateTime("field", "2024-06-22 09:00:00.234", "yyyy-MM-dd HH:mm:ss.SSS"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseZonedDateTime("field", "2024-06-22 13:55:01", "yyyy-MM-dd HH:mm:ss"));

        var now = ZonedDateTime.now();
        assertEquals(now, GenericUtils.parseZonedDateTime("field", now, null));

        ZonedDateTime date = GenericUtils.parseZonedDateTime("field", "2024-06-22T13:55:01+02:00", "yyyy-MM-dd'T'HH:mm:ssXXXXX");
        assertNotNull(date);
        assertEquals(Month.JUNE, date.getMonth());
        assertEquals(2024, date.getYear());
        assertEquals(22, date.getDayOfMonth());
        assertEquals(13, date.getHour());
        assertEquals(55, date.getMinute());
        assertEquals(1, date.getSecond());
        assertEquals(ZoneOffset.of("+02:00"), date.getOffset());
        assertEquals(ZoneOffset.of("+02:00"), date.getZone());

        date = GenericUtils.parseZonedDateTime("field", "2024-06-22T09:00:00.234+01:00", "yyyy-MM-dd'T'HH:mm:ss.SSSXXXXX");
        assertNotNull(date);
        assertEquals(Month.JUNE, date.getMonth());
        assertEquals(2024, date.getYear());
        assertEquals(22, date.getDayOfMonth());
        assertEquals(9, date.getHour());
        assertEquals(0, date.getMinute());
        assertEquals(0, date.getSecond());
        assertEquals(234000000, date.getNano());
        assertEquals(ZoneOffset.of("+01:00"), date.getOffset());
        assertEquals(ZoneOffset.of("+01:00"), date.getZone());
    }

    @Test
    public void parseLocalTimeTest() {
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseLocalTime("field", 20240622152200L, "yyyy-MM-dd"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseLocalTime("field", "22/06/2024 15:01:02", "yyyy-MM-dd"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseLocalTime("field", "2024-06-22", "yyyy-MM-dd"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseLocalTime("field", "20240622", "yyyyMMdd"));

        var now = LocalTime.now();
        assertEquals(now, GenericUtils.parseLocalTime("field", now, null));

        LocalTime date = GenericUtils.parseLocalTime("field", "2024-06-22 13:55:01", "yyyy-MM-dd HH:mm:ss");
        assertNotNull(date);
        assertEquals(13, date.getHour());
        assertEquals(55, date.getMinute());
        assertEquals(1, date.getSecond());

        date = GenericUtils.parseLocalTime("field", "13:55:01", "HH:mm:ss");
        assertNotNull(date);
        assertEquals(13, date.getHour());
        assertEquals(55, date.getMinute());
        assertEquals(1, date.getSecond());
    }

    @Test
    public void parseOffsetTime() {
        assertThrows(InvalidValueException.class, () -> GenericUtils.parseOffsetTime("field", 20240622152200L, "yyyy-MM-dd"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseOffsetTime("field", "22/06/2024 15:01:02", "yyyy-MM-dd"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseOffsetTime("field", "2024-06-22", "yyyy-MM-dd"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseOffsetTime("field", "20240622", "yyyyMMdd"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseOffsetTime("field", "2024-06-22 09:00:00.234", "yyyy-MM-dd HH:mm:ss.SSS"));
        assertThrows(DateTimeParseException.class, () -> GenericUtils.parseOffsetTime("field", "2024-06-22 13:55:01", "yyyy-MM-dd HH:mm:ss"));

        var now = OffsetTime.now();
        assertEquals(now, GenericUtils.parseOffsetTime("field", now, null));

        OffsetTime date = GenericUtils.parseOffsetTime("field", "2024-06-22T13:55:01+02:00", "yyyy-MM-dd'T'HH:mm:ssXXXXX");
        assertNotNull(date);
        assertEquals(13, date.getHour());
        assertEquals(55, date.getMinute());
        assertEquals(1, date.getSecond());
        assertEquals(ZoneOffset.of("+02:00"), date.getOffset());

        date = GenericUtils.parseOffsetTime("field", "2024-06-22T09:00:00.234+01:00", "yyyy-MM-dd'T'HH:mm:ss.SSSXXXXX");
        assertNotNull(date);
        assertEquals(9, date.getHour());
        assertEquals(0, date.getMinute());
        assertEquals(0, date.getSecond());
        assertEquals(234000000, date.getNano());
        assertEquals(ZoneOffset.of("+01:00"), date.getOffset());

        date = GenericUtils.parseOffsetTime("field", "09:00:00.234+01:00", "HH:mm:ss.SSSXXXXX");
        assertNotNull(date);
        assertEquals(9, date.getHour());
        assertEquals(0, date.getMinute());
        assertEquals(0, date.getSecond());
        assertEquals(234000000, date.getNano());
        assertEquals(ZoneOffset.of("+01:00"), date.getOffset());
    }

}
