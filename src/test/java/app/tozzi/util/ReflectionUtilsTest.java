package app.tozzi.util;

import app.tozzi.annotation.Searchable;
import app.tozzi.entity.MyEntity;
import app.tozzi.entity.TestEntity1;
import app.tozzi.entity.TestEntity6;
import app.tozzi.exception.JPASearchException;
import app.tozzi.model.*;
import jakarta.persistence.*;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.*;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

public class ReflectionUtilsTest {

    @Test
    public void getAllProjectableFields() {
        var projectableFields = ReflectionUtils.getAllProjectableFields(MyModel.class);
        assertNotNull(projectableFields);
        assertFalse(projectableFields.isEmpty());
        assertEquals(7, projectableFields.size());
    }

    @Test
    public void getAllSearchableFields() {
        var searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);
        assertNotNull(searchableFields);
        assertFalse(searchableFields.isEmpty());
        assertEquals(32, searchableFields.size());
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("id") && e.getValue().getKey().sortable() && e.getValue().getKey().targetType().equals(JPASearchType.LONG) && e.getValue().getValue().getType().equals(String.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("uuid") && e.getValue().getKey().sortable() && e.getValue().getKey().targetType().equals(JPASearchType.UNTYPED) && e.getValue().getValue().getType().equals(UUID.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("myEnum") && e.getValue().getKey().sortable() && e.getValue().getKey().targetType().equals(JPASearchType.ENUM) && e.getValue().getValue().getType().equals(MyEnum.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("stringOne") && e.getValue().getKey().sortable() && e.getValue().getKey().targetType().equals(JPASearchType.UNTYPED) && e.getValue().getValue().getType().equals(String.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("stringTwo") && !e.getValue().getKey().sortable() && e.getValue().getValue().getType().equals(String.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("stringThree") && e.getValue().getKey().trim() && !e.getValue().getKey().sortable() && e.getValue().getValue().getType().equals(String.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("stringFalse") && !e.getValue().getKey().allowLikeFilters() && e.getValue().getValue().getType().equals(String.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("stringMail") && e.getValue().getKey().entityFieldKey().equals("email") && e.getValue().getKey().regexPattern().equals("^[a-zA-Z0-9_!#$%&’*+/=?`{|}~^.-]+@[a-zA-Z0-9.-]+$") && e.getValue().getValue().getType().equals(String.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("primitiveInteger") && e.getValue().getKey().minSize() == 5 && e.getValue().getKey().maxSize() == 10 && e.getValue().getValue().getType().equals(int.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("wrapperInteger") && e.getValue().getKey().minDigits() == 2 && e.getValue().getKey().maxDigits() == 4 && e.getValue().getValue().getType().equals(Integer.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("stringDate") && e.getValue().getKey().targetType().equals(JPASearchType.DATE) && e.getValue().getKey().datePattern().equals("yyyyMMdd") && e.getValue().getValue().getType().equals(String.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("dateOne") && e.getValue().getKey().datePattern().equals("yyyy-MM-dd HH:mm:ss") && Stream.of(e.getValue().getKey().allowedFilters()).allMatch(af -> af.equals(JPASearchOperatorFilter.BETWEEN) || af.equals(JPASearchOperatorFilter.GT)) && e.getValue().getValue().getType().equals(Date.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("primitiveLong") && e.getValue().getValue().getType().equals(long.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("wrapperLong") && e.getValue().getKey().tags().length == 2 && Stream.of(e.getValue().getKey().tags()).allMatch(t -> (t.fieldKey().equals("wrapperLong.one") && t.entityFieldKey().isEmpty()) || ((t.fieldKey().equals("wrapperLong.two") && t.entityFieldKey().equals("wrapperLongYes")))) && e.getValue().getValue().getType().equals(Long.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("primitiveFloat") && e.getValue().getKey().decimalFormat().equals("#.###") && e.getValue().getValue().getType().equals(float.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("wrapperFloat") && e.getValue().getKey().decimalFormat().equals("#.##") && e.getValue().getValue().getType().equals(Float.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("primitiveDouble") && e.getValue().getKey().entityFieldKey().equals("primitiveDoubleYes") && e.getValue().getValue().getType().equals(double.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("wrapperDouble") && e.getValue().getKey().decimalFormat().equals("#.000") && e.getValue().getValue().getType().equals(Double.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("bigDecimal") && e.getValue().getKey().minDigits() == -1 && e.getValue().getKey().maxDigits() == -1 && e.getValue().getKey().maxSize() == -1 && e.getValue().getKey().minSize() == -1 && e.getValue().getValue().getType().equals(BigDecimal.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("localDateTime") && e.getValue().getKey().datePattern().equals("yyyy-MM-dd'T'HH:mm:ss") && Stream.of(e.getValue().getKey().notAllowedFilters()).allMatch(af -> af.equals(JPASearchOperatorFilter.LT)) && e.getValue().getValue().getType().equals(LocalDateTime.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("localDate") && e.getValue().getKey().datePattern().equals("yyyy-MM-dd") && e.getValue().getValue().getType().equals(LocalDate.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("localTime") && e.getValue().getKey().datePattern().equals("HHmmssXXX") && e.getValue().getValue().getType().equals(LocalTime.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("offsetDateTime") && e.getValue().getKey().datePattern().equals("yyyy-MM-dd'T'HH:mm:ssXXX") && e.getValue().getValue().getType().equals(OffsetDateTime.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("zonedDateTime") && e.getValue().getKey().datePattern().equals("yyyy-MM-dd'T'HH:mm:ss.SSSXXXXX") && e.getValue().getValue().getType().equals(ZonedDateTime.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("offsetTime") && e.getValue().getKey().datePattern().equals("HH:mm:ss.SSSXXXXX") && !e.getValue().getKey().sortable() && e.getValue().getValue().getType().equals(OffsetTime.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("primitiveBoolean") && e.getValue().getKey().tags().length == 0 && e.getValue().getKey().minDigits() == -1 && e.getValue().getKey().maxDigits() == -1 && e.getValue().getKey().maxSize() == -1 && e.getValue().getKey().minSize() == -1 && e.getValue().getValue().getType().equals(boolean.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("wrapperBoolean") && e.getValue().getKey().tags().length == 0 && e.getValue().getKey().minDigits() == -1 && e.getValue().getKey().maxDigits() == -1 && e.getValue().getKey().maxSize() == -1 && e.getValue().getKey().minSize() == -1 && e.getValue().getValue().getType().equals(Boolean.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("mySubModel.searchMe") && e.getValue().getKey().tags().length == 0 && e.getValue().getKey().minDigits() == -1 && e.getValue().getKey().maxDigits() == -1 && e.getValue().getKey().maxSize() == -1 && e.getValue().getKey().minSize() == -1 && e.getValue().getValue().getType().equals(String.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("mySubModel.mySubSubModel.searchMeAgain") && e.getValue().getKey().entityFieldKey().equals("test1.colTest1") && e.getValue().getKey().tags().length == 0 && e.getValue().getKey().minDigits() == -1 && e.getValue().getKey().maxDigits() == -1 && e.getValue().getKey().maxSize() == -1 && e.getValue().getKey().minSize() == -1 && e.getValue().getValue().getType().equals(String.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("list.other") && e.getValue().getValue().getType().equals(String.class)));
        assertTrue(searchableFields.entrySet().stream().anyMatch(e -> e.getKey().equals("keywords") && e.getValue().getKey().elementCollection()));
        assertFalse(searchableFields.containsKey("notSearchableOne"));
        assertFalse(searchableFields.containsKey("notSearchableTwo"));
        assertFalse(searchableFields.containsKey("notSearchableThree"));
        assertFalse(searchableFields.containsKey("mySubModel.notSearchable"));
        assertFalse(searchableFields.containsKey("mySubModel.mySubSubModel"));
        assertFalse(searchableFields.containsKey("mySubModel.mySubSubModel.notSearchableNo"));
    }

    @Test
    public void recursiveModelTest() {
        var map = ReflectionUtils.getAllSearchableFields(RecursiveModel.class);
        assertNotNull(map);
        assertTrue(map.containsKey("name"));
        assertTrue(map.containsKey("predecessor.name"));
        assertEquals(34, map.size());
    }

    @Test
    public void inheritanceTest() {
        var map = ReflectionUtils.getAllSearchableFields(ModelB.class);
        assertNotNull(map);
        assertTrue(map.containsKey("modelBID"));
        assertTrue(map.containsKey("modelBField"));
        assertTrue(map.containsKey("modelAID"));
        assertTrue(map.containsKey("modelAField"));
        assertEquals(4, map.size());
    }

    @Test
    public void getIdFields_MyEntity() {
        var idFields = ReflectionUtils.getIdFields(MyEntity.class);

        assertNotNull(idFields);
        assertFalse(idFields.isEmpty());

        assertTrue(idFields.containsKey(MyEntity.class));
        var rootIds = idFields.get(MyEntity.class);
        assertTrue(rootIds.containsKey("id"));

        assertTrue(idFields.containsKey(TestEntity1.class));
        var t1Ids = idFields.get(TestEntity1.class);
        assertTrue(t1Ids.containsKey("test1.id"));

        assertTrue(idFields.containsKey(TestEntity6.class));
        var t6Ids = idFields.get(TestEntity6.class);
        assertTrue(t6Ids.containsKey("test1.entity6s.id"));
    }

    @Test
    public void getIdFields_EmbeddedId() {
        var idFields = ReflectionUtils.getIdFields(EntityWithEmbedded.class);

        assertNotNull(idFields);
        assertTrue(idFields.containsKey(EntityWithEmbedded.class));

        var fields = idFields.get(EntityWithEmbedded.class);
        assertEquals(2, fields.size());
        assertTrue(fields.containsKey("pk.code"));
        assertTrue(fields.containsKey("pk.sequence"));
    }

    @Test
    public void getIdFields_MappedSuperclass() {
        var idFields = ReflectionUtils.getIdFields(ChildEntity.class);

        assertTrue(idFields.containsKey(ChildEntity.class));
        var fields = idFields.get(ChildEntity.class);

        assertTrue(fields.containsKey("id"));
        assertEquals(1, fields.size());
    }

    @Test
    public void invalidCollectionTypeTest() {
        var ex = assertThrows(JPASearchException.class, () -> ReflectionUtils.getAllSearchableFields(BadCollectionModel.class));
        assertTrue(ex.getMessage().contains("Invalid searchable type"));
    }

    @Entity
    static class EntityWithEmbedded {
        @EmbeddedId
        private MyEmbeddableId pk;
    }

    @Embeddable
    static class MyEmbeddableId {
        private String code;
        private Long sequence;
    }

    @MappedSuperclass
    static class BaseEntity {
        @Id
        private Long id;
    }

    @Entity
    static class ChildEntity extends BaseEntity {
        private String childField;
    }

    static class BadCollectionModel {
        @Searchable
        private List rawList;
    }
}