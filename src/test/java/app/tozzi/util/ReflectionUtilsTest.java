package app.tozzi.util;

import app.tozzi.annotation.Searchable;
import app.tozzi.model.JPASearchOperatorFilter;
import app.tozzi.model.JPASearchType;
import app.tozzi.model.MyModel;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.*;
import java.util.Date;
import java.util.Map;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

public class ReflectionUtilsTest {

    @Test
    public void getAllSearchableFieldsTest() {
        Map<String, Pair<Searchable, Class<?>>> searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);
        assertNotNull(searchableFields);
        assertFalse(searchableFields.isEmpty());
        assertEquals(27, searchableFields.size());
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("id") && e.getValue().getKey().sortable() && e.getValue().getKey().targetType().equals(JPASearchType.LONG) && e.getValue().getValue().equals(String.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("stringOne") && e.getValue().getKey().sortable() && e.getValue().getKey().targetType().equals(JPASearchType.UNTYPED) && e.getValue().getValue().equals(String.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("stringTwo") && !e.getValue().getKey().sortable() && e.getValue().getValue().equals(String.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("stringThree") && e.getValue().getKey().trim() && !e.getValue().getKey().sortable() && e.getValue().getValue().equals(String.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("stringFalse") && !e.getValue().getKey().allowLikeFilters() && e.getValue().getValue().equals(String.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("stringMail") && e.getValue().getKey().entityFieldKey().equals("email") && e.getValue().getKey().regexPattern().equals("^[a-zA-Z0-9_!#$%&’*+/=?`{|}~^.-]+@[a-zA-Z0-9.-]+$") && e.getValue().getValue().equals(String.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("primitiveInteger") && e.getValue().getKey().minSize() == 5 && e.getValue().getKey().maxSize() == 10 && e.getValue().getValue().equals(int.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("wrapperInteger") && e.getValue().getKey().minDigits() == 2 && e.getValue().getKey().maxDigits() == 4 && e.getValue().getValue().equals(Integer.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("stringDate") && e.getValue().getKey().targetType().equals(JPASearchType.DATE) && e.getValue().getKey().datePattern().equals("yyyyMMdd") && e.getValue().getValue().equals(String.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("dateOne")  && e.getValue().getKey().datePattern().equals("yyyy-MM-dd HH:mm:ss") && Stream.of(e.getValue().getKey().allowedFilters()).allMatch(af -> af.equals(JPASearchOperatorFilter.BETWEEN) || af.equals(JPASearchOperatorFilter.GT)) && e.getValue().getValue().equals(Date.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("primitiveLong") && e.getValue().getValue().equals(long.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("wrapperLong") && e.getValue().getKey().tags().length == 2 && Stream.of(e.getValue().getKey().tags()).allMatch(t -> (t.fieldKey().equals("wrapperLong.one") && t.entityFieldKey().isEmpty()) || ((t.fieldKey().equals("wrapperLong.two") && t.entityFieldKey().equals("wrapperLongYes")))) && e.getValue().getValue().equals(Long.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("primitiveFloat") && e.getValue().getKey().decimalFormat().equals("#.###") && e.getValue().getValue().equals(float.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("wrapperFloat") && e.getValue().getKey().decimalFormat().equals("#.##") && e.getValue().getValue().equals(Float.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("primitiveDouble") && e.getValue().getKey().entityFieldKey().equals("primitiveDoubleYes") && e.getValue().getValue().equals(double.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("wrapperDouble") && e.getValue().getKey().decimalFormat().equals("#.000") && e.getValue().getValue().equals(Double.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("bigDecimal") && e.getValue().getKey().minDigits() == -1 && e.getValue().getKey().maxDigits() == -1 && e.getValue().getKey().maxSize() == -1 && e.getValue().getKey().minSize() == -1 && e.getValue().getValue().equals(BigDecimal.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("localDateTime")  && e.getValue().getKey().datePattern().equals("yyyy-MM-dd'T'HH:mm:ss") && Stream.of(e.getValue().getKey().notAllowedFilters()).allMatch(af -> af.equals(JPASearchOperatorFilter.LT)) && e.getValue().getValue().equals(LocalDateTime.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("localDate")  && e.getValue().getKey().datePattern().equals("yyyy-MM-dd") && e.getValue().getValue().equals(LocalDate.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("localTime")  && e.getValue().getKey().datePattern().equals("HHmmssXXX") && e.getValue().getValue().equals(LocalTime.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("offsetDateTime")  && e.getValue().getKey().datePattern().equals("yyyy-MM-dd'T'HH:mm:ssXXX") && e.getValue().getValue().equals(OffsetDateTime.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("zonedDateTime")  && e.getValue().getKey().datePattern().equals("yyyy-MM-dd'T'HH:mm:ss.SSSXXXXX") && e.getValue().getValue().equals(ZonedDateTime.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("offsetTime")  && e.getValue().getKey().datePattern().equals("HH:mm:ss.SSSXXXXX") && !e.getValue().getKey().sortable() && e.getValue().getValue().equals(OffsetTime.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("primitiveBoolean") && e.getValue().getKey().tags().length == 0 && e.getValue().getKey().minDigits() == -1 && e.getValue().getKey().maxDigits() == -1 && e.getValue().getKey().maxSize() == -1 && e.getValue().getKey().minSize() == -1 && e.getValue().getValue().equals(boolean.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("wrapperBoolean") && e.getValue().getKey().tags().length == 0 && e.getValue().getKey().minDigits() == -1 && e.getValue().getKey().maxDigits() == -1 && e.getValue().getKey().maxSize() == -1 && e.getValue().getKey().minSize() == -1 && e.getValue().getValue().equals(Boolean.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("mySubModel.searchMe") && e.getValue().getKey().tags().length == 0 && e.getValue().getKey().minDigits() == -1 && e.getValue().getKey().maxDigits() == -1 && e.getValue().getKey().maxSize() == -1 && e.getValue().getKey().minSize() == -1 && e.getValue().getValue().equals(String.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("mySubModel.mySubSubModel.searchMeAgain") && e.getValue().getKey().entityFieldKey().equals("test1.colTest1") && e.getValue().getKey().tags().length == 0 && e.getValue().getKey().minDigits() == -1 && e.getValue().getKey().maxDigits() == -1 && e.getValue().getKey().maxSize() == -1 && e.getValue().getKey().minSize() == -1 && e.getValue().getValue().equals(String.class)));
        assertFalse(searchableFields.containsKey("notSearchableOne"));
        assertFalse(searchableFields.containsKey("notSearchableTwo"));
        assertFalse(searchableFields.containsKey("notSearchableThree"));
        assertFalse(searchableFields.containsKey("mySubModel.notSearchable"));
        assertFalse(searchableFields.containsKey("mySubModel.mySubSubModel"));
        assertFalse(searchableFields.containsKey("mySubModel.mySubSubModel.notSearchableNo"));

    }
}
