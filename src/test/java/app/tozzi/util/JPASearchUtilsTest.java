package app.tozzi.util;

import app.tozzi.entity.MyEntity;
import app.tozzi.exception.JPASearchException;
import app.tozzi.model.input.JPASearchInput;
import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.*;
import org.hibernate.query.sqm.tree.domain.SqmBasicValuedSimplePath;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.persistence.autoconfigure.EntityScan;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.ContextConfiguration;

import java.math.BigDecimal;
import java.time.*;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

@DataJpaTest
@EnableAutoConfiguration
@ContextConfiguration(classes = {JPASearchUtilsTest.class})
@EntityScan("app.tozzi.entity")
public class JPASearchUtilsTest {

    @Autowired
    private EntityManager entityManager;

    @Test
    public void fetchManagement() {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MyEntity> criteriaQuery = criteriaBuilder.createQuery(MyEntity.class);
        Root<MyEntity> root = criteriaQuery.from(MyEntity.class);
        Map<String, JoinType> fetches =
                Map.of("test1", JoinType.LEFT, "test2.entities4.entity5", JoinType.LEFT, "test2", JoinType.INNER, "test3.entities5", JoinType.RIGHT);

        root = JPASearchUtils.fetchManagement(fetches, root);
        assertNotNull(root.getFetches());
        assertFalse(root.getFetches().isEmpty());
        assertEquals(3, root.getFetches().size());
        assertTrue(root.getFetches().stream().anyMatch(f -> f.getAttribute().getName().equals("test1") && f.getJoinType().equals(JoinType.LEFT)));
        assertTrue(root.getFetches().stream().anyMatch(f -> f.getAttribute().getName().equals("test2") && f.getJoinType().equals(JoinType.INNER)
                && f.getFetches() != null && !f.getFetches().isEmpty() && f.getFetches().size() == 1
                && f.getFetches().stream().toList().get(0).getAttribute().getName().equals("entities4")
                && f.getFetches().stream().toList().get(0).getJoinType().equals(JoinType.LEFT)
                && f.getFetches().stream().toList().get(0).getFetches().iterator().next().getAttribute().getName().equals("entity5")
                && f.getFetches().stream().toList().get(0).getFetches().iterator().next().getJoinType().equals(JoinType.LEFT)
        ));
        assertTrue(root.getFetches().stream().anyMatch(f -> f.getAttribute().getName().equals("test3") && f.getJoinType().equals(JoinType.RIGHT)
                && f.getFetches() != null && !f.getFetches().isEmpty() && f.getFetches().size() == 1
                && f.getFetches().iterator().next().getAttribute().getName().equals("entities5")
        ));
    }

    @Test
    public void getPath() {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MyEntity> criteriaQuery = criteriaBuilder.createQuery(MyEntity.class);
        Root<MyEntity> root = criteriaQuery.from(MyEntity.class);
        Expression<?> path = JPASearchUtils.getPath(root, "id");
        assertNotNull(path);
        assertEquals("id", ((SqmBasicValuedSimplePath) path).getNavigablePath().getLocalName());

        path = JPASearchUtils.getPath(root, "notSearchableThree");
        assertNotNull(path);
        assertEquals("notSearchableThree", ((SqmBasicValuedSimplePath) path).getNavigablePath().getLocalName());

        path = JPASearchUtils.getPath(root, "test1.colTest1");
        assertNotNull(path);
        assertEquals("colTest1", ((SqmBasicValuedSimplePath) path).getNavigablePath().getLocalName());
        assertEquals("test1", ((SqmBasicValuedSimplePath) path).getNavigablePath().getParent().getLocalName());

        path = JPASearchUtils.getPath(root, "test2.entities4.colTest4");
        assertNotNull(path);
        assertEquals("colTest4", ((SqmBasicValuedSimplePath) path).getNavigablePath().getLocalName());
        assertEquals("entities4", ((SqmBasicValuedSimplePath) path).getNavigablePath().getParent().getParent().getLocalName());
        assertEquals("test2", ((SqmBasicValuedSimplePath) path).getNavigablePath().getParent().getParent().getParent().getLocalName());

    }

    @Test
    public void mode1ToMode2_1() {
        Map<String, String> filters = generateRandomMap();
        filters.put("_limit", "10");
        filters.put("_offset", "1");
        filters.put("id_sort", "DESC");
        var input = JPASearchUtils.toObject(filters, true, true, false);
        assertNotNull(input);
        assertNotNull(input.getOptions());
        assertEquals(1, input.getOptions().getPageOffset());
        assertEquals(10, input.getOptions().getPageSize());
        assertNotNull(input.getOptions().getSortOptions());
        assertEquals("id", input.getOptions().getSortOptions().get(0).getKey());
        assertEquals(true, input.getOptions().getSortOptions().get(0).getDesc());
        assertNotNull(input.getFilter());
        assertEquals("and", input.getFilter().getOperator());
        assertNotNull(input.getFilter().getFilters());
        assertFalse(input.getFilter().getFilters().isEmpty());
        assertEquals(17, input.getFilter().getFilters().size());
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.FilterSingleValue fsv && fsv.getKey().equals("id") && fsv.getOperator().equals("eq") && fsv.getValue().equals(filters.get("id_eq")) && fsv.getOptions() == null));
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.FilterSingleValue fsv && fsv.getKey().equals("stringOne") && fsv.getOperator().equals("contains") && fsv.getValue().equals(filters.get("stringOne_contains")) && fsv.getOptions() == null));
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.FilterSingleValue fsv && fsv.getKey().equals("stringThree") && fsv.getOperator().equals("startsWith") && fsv.getValue().equals(filters.get("stringThree_startsWith#i")) && fsv.getOptions() != null && fsv.getOptions().isIgnoreCase()));
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.FilterSingleValue fsv && fsv.getKey().equals("stringFalse") && fsv.getOperator().equals("eq") && fsv.getValue().equals(filters.get("stringFalse_eq#n")) && fsv.getOptions() != null && fsv.getOptions().isNegate()));
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.FilterMultipleValues fsv && fsv.getKey().equals("stringTwo") && fsv.getOperator().equals("in") && fsv.getValues().contains("test1") && fsv.getValues().contains("test2,test3")));
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.FilterMultipleValues fsv && fsv.getKey().equals("primitiveFloat") && fsv.getOperator().equals("between") && fsv.getValues().contains("1.001") && fsv.getValues().contains("1.002")));
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.FilterSingleValue fsv && fsv.getKey().equals("searchMeAgain") && fsv.getOperator().equals("eq") && fsv.getValue().equals(filters.get("searchMeAgain_eq#i#n")) && fsv.getOptions() != null && fsv.getOptions().isNegate() && fsv.getOptions().isIgnoreCase()));
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.FilterSingleValue fsv && fsv.getKey().equals("stringTwo") && fsv.getOperator().equals("eq") && fsv.getValue().equals("Via Roma,1")));
    }

    @Test
    public void mode1ToMode2_2() {
        Map<String, String> filters = generateRandomMap();
        filters.put("_limit", "10");
        filters.put("_offset", "1");
        filters.put("id_sort", "ASC");
        var input = JPASearchUtils.toObject(filters, true, true, false);
        assertNotNull(input);
        assertNotNull(input.getOptions());
        assertEquals(1, input.getOptions().getPageOffset());
        assertEquals(10, input.getOptions().getPageSize());
        assertNotNull(input.getOptions().getSortOptions());
        assertEquals("id", input.getOptions().getSortOptions().get(0).getKey());
        assertEquals(false, input.getOptions().getSortOptions().get(0).getDesc());
        assertNotNull(input.getFilter());
        assertEquals("and", input.getFilter().getOperator());
        assertNotNull(input.getFilter().getFilters());
        assertFalse(input.getFilter().getFilters().isEmpty());
        assertEquals(17, input.getFilter().getFilters().size());
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.FilterSingleValue fsv && fsv.getKey().equals("id") && fsv.getOperator().equals("eq") && fsv.getValue().equals(filters.get("id_eq")) && fsv.getOptions() == null));
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.FilterSingleValue fsv && fsv.getKey().equals("stringOne") && fsv.getOperator().equals("contains") && fsv.getValue().equals(filters.get("stringOne_contains")) && fsv.getOptions() == null));
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.FilterSingleValue fsv && fsv.getKey().equals("stringThree") && fsv.getOperator().equals("startsWith") && fsv.getValue().equals(filters.get("stringThree_startsWith#i")) && fsv.getOptions() != null && fsv.getOptions().isIgnoreCase()));
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.FilterSingleValue fsv && fsv.getKey().equals("stringFalse") && fsv.getOperator().equals("eq") && fsv.getValue().equals(filters.get("stringFalse_eq#n")) && fsv.getOptions() != null && fsv.getOptions().isNegate()));
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.FilterMultipleValues fsv && fsv.getKey().equals("stringTwo") && fsv.getOperator().equals("in") && fsv.getValues().contains("test1") && fsv.getValues().contains("test2,test3")));
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.FilterMultipleValues fsv && fsv.getKey().equals("primitiveFloat") && fsv.getOperator().equals("between") && fsv.getValues().contains("1.001") && fsv.getValues().contains("1.002")));
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.FilterSingleValue fsv && fsv.getKey().equals("searchMeAgain") && fsv.getOperator().equals("eq") && fsv.getValue().equals(filters.get("searchMeAgain_eq#i#n")) && fsv.getOptions() != null && fsv.getOptions().isNegate() && fsv.getOptions().isIgnoreCase()));
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.FilterSingleValue fsv && fsv.getKey().equals("stringTwo") && fsv.getOperator().equals("eq") && fsv.getValue().equals("Via Roma,1")));
    }

    @Test
    public void emptyFilters_1() {
        var empty1 = JPASearchUtils.toObject(null, false, false, false);
        assertNotNull(empty1);
        assertNull(empty1.getFilter());
        var empty2 = JPASearchUtils.toObject(Collections.emptyMap(), false, false, false);
        assertNotNull(empty2);
        assertNull(empty2.getFilter());
    }

    @Test
    public void emptyFilters_2() {
        assertThrows(JPASearchException.class, () -> JPASearchUtils.toObject(null, true, true, false));
        assertThrows(JPASearchException.class, () -> JPASearchUtils.toObject(null, false, true, false));
        assertThrows(JPASearchException.class, () -> JPASearchUtils.toObject(null, true, false, false));
        assertThrows(JPASearchException.class, () -> JPASearchUtils.toObject(Collections.emptyMap(), true, true, false));
        assertThrows(JPASearchException.class, () -> JPASearchUtils.toObject(Collections.emptyMap(), false, true, false));
        assertThrows(JPASearchException.class, () -> JPASearchUtils.toObject(Collections.emptyMap(), true, false, false));
    }

    private static Map<String, String> generateRandomMap() {
        Map<String, String> resultMap = new HashMap<>();
        resultMap.put("id_eq", String.valueOf(generateRandomValue(String.class)));
        resultMap.put("stringOne_contains", String.valueOf(generateRandomValue(String.class)));
        resultMap.put("stringTwo_in", "test1,test2/,test3");
        resultMap.put("stringThree_startsWith#i", String.valueOf(generateRandomValue(String.class)));
        resultMap.put("stringFalse_eq#n", String.valueOf(generateRandomValue(String.class)));
        resultMap.put("email_eq", String.valueOf(generateRandomValue(String.class)));
        resultMap.put("primitiveInteger_gt", String.valueOf(generateRandomValue(int.class)));
        resultMap.put("wrapperInteger_gte", String.valueOf(generateRandomValue(Integer.class)));
        resultMap.put("stringDate_lte", String.valueOf(generateRandomValue(String.class)));
        resultMap.put("dateOne_lt", String.valueOf(generateRandomValue(Date.class)));
        resultMap.put("primitiveLong_eq", String.valueOf(generateRandomValue(long.class)));
        resultMap.put("wrapperLong.one_eq", String.valueOf(generateRandomValue(Long.class)));
        resultMap.put("primitiveFloat_between", "1.001,1.002");
        resultMap.put("wrapperFloat_null", "true");
        resultMap.put("searchMe_endsWith", String.valueOf(generateRandomValue(String.class)));
        resultMap.put("searchMeAgain_eq#i#n", String.valueOf(generateRandomValue(String.class)));
        resultMap.put("stringTwo_eq", "Via Roma/,1");
        return resultMap;
    }

    private static Object generateRandomValue(Class<?> fieldType) {
        Random random = new Random();

        if (fieldType == String.class) {
            return "RandomString" + random.nextInt(100);
        } else if (fieldType == int.class || fieldType == Integer.class) {
            return random.nextInt(100);
        } else if (fieldType == long.class || fieldType == Long.class) {
            return random.nextLong();
        } else if (fieldType == float.class || fieldType == Float.class) {
            return random.nextFloat();
        } else if (fieldType == double.class || fieldType == Double.class) {
            return random.nextDouble();
        } else if (fieldType == BigDecimal.class) {
            return BigDecimal.valueOf(random.nextDouble());
        } else if (fieldType == Date.class) {
            return new Date();
        } else if (fieldType == LocalDateTime.class) {
            return LocalDateTime.now().minusDays(random.nextInt(365));
        } else if (fieldType == LocalDate.class) {
            return LocalDate.now().minusDays(random.nextInt(365));
        } else if (fieldType == LocalTime.class) {
            return LocalTime.now().minusHours(random.nextInt(24));
        } else if (fieldType == OffsetDateTime.class) {
            return OffsetDateTime.now().minusDays(random.nextInt(365));
        } else if (fieldType == ZonedDateTime.class) {
            return ZonedDateTime.now().minusDays(random.nextInt(365));
        } else if (fieldType == OffsetTime.class) {
            return OffsetTime.now().minusHours(random.nextInt(24));
        } else if (fieldType == boolean.class || fieldType == Boolean.class) {
            return random.nextBoolean();
        }

        return null;
    }


}
