package app.tozzi.core;

import app.tozzi.entity.*;
import app.tozzi.model.MyModel;
import app.tozzi.model.input.JPASearchInput;
import app.tozzi.repository.MyRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;

import java.math.BigDecimal;
import java.time.*;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

@DataJpaTest
@EnableAutoConfiguration
@ContextConfiguration(classes = {JPASearchCoreTest.class})
@EntityScan("app.tozzi.entity")
@TestPropertySource(properties = {
        "spring.jpa.show-sql=true",
        "logging.level.org.hibernate.SQL=DEBUG",
        "logging.level.org.hibernate.type.descriptor.sql.BasicBinder=TRACE"
})
@EnableJpaRepositories("app.tozzi.repository")
public class JPASearchCoreTest {

    @Autowired
    private MyRepository myRepository;

    @BeforeEach
    void init() {
        setUp();
    }

    @Test
    public void simpleORCoreTest1() {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setFilters(new ArrayList<>());
        root.setOperator("or");
        var ff1 = new JPASearchInput.FilterSingleValue();
        ff1.setKey("id");
        ff1.setValue("1");
        ff1.setOperator("eq");
        root.getFilters().add(ff1);
        var ff2 = new JPASearchInput.FilterSingleValue();
        ff2.setKey("id");
        ff2.setValue("2");
        ff2.setOperator("eq");
        root.getFilters().add(ff2);
        input.setFilter(root);
        List<MyEntity> res = myRepository.findAll(input, MyModel.class);
        assertNotNull(res);
        assertFalse(res.isEmpty());
        assertEquals(2, res.size());
        assertTrue(res.stream().anyMatch(r -> r.getId().equals(1L)));
        assertTrue(res.stream().anyMatch(r -> r.getId().equals(2L)));
    }

    @Test
    public void simpleORCoreTest2() {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setFilters(new ArrayList<>());
        root.setOperator("or");
        var ff1 = new JPASearchInput.FilterSingleValue();
        ff1.setKey("id");
        ff1.setValue("1");
        ff1.setOperator("eq");
        root.getFilters().add(ff1);
        var ff2 = new JPASearchInput.FilterSingleValue();
        ff2.setKey("stringMail");
        ff2.setValue("email2@example.com");
        ff2.setOperator("eq");
        root.getFilters().add(ff2);
        input.setFilter(root);
        List<MyEntity> res = myRepository.findAll(input, MyModel.class);
        assertNotNull(res);
        assertFalse(res.isEmpty());
        assertEquals(2, res.size());
        assertTrue(res.stream().anyMatch(r -> r.getId().equals(1L)));
        assertTrue(res.stream().anyMatch(r -> r.getId().equals(2L) && r.getEmail().equals("email2@example.com")));
    }

    @Test
    public void complexTest1() {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setFilters(new ArrayList<>());
        root.setOperator("and");

        var ff1 = new JPASearchInput.FilterSingleValue();
        ff1.setKey("stringOne");
        ff1.setValue("stringone");
        ff1.setOperator("startsWith");
        ff1.setOptions(new JPASearchInput.JPASearchFilterOptions());
        ff1.getOptions().setIgnoreCase(true);
        root.getFilters().add(ff1);

        var ff2 = new JPASearchInput.FilterSingleValue();
        ff2.setKey("stringTwo");
        ff2.setValue("two");
        ff2.setOperator("contains");
        ff2.setOptions(new JPASearchInput.JPASearchFilterOptions());
        ff2.getOptions().setIgnoreCase(true);
        root.getFilters().add(ff2);

        var ff3 = new JPASearchInput.FilterMultipleValues();
        ff3.setKey("stringMail");
        ff3.setValues(List.of("email1@example.com", "email2@example.com"));
        ff3.setOperator("in");
        root.getFilters().add(ff3);
        input.setFilter(root);
        List<MyEntity> res = myRepository.findAll(input, MyModel.class);
        assertNotNull(res);
        assertFalse(res.isEmpty());
        assertEquals(2, res.size());
        assertTrue(res.stream().anyMatch(r -> r.getId().equals(1L) && r.getEmail().equals("email1@example.com")));
        assertTrue(res.stream().anyMatch(r -> r.getId().equals(2L) && r.getEmail().equals("email2@example.com")));
    }

    @Test
    public void complexTest2() {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setFilters(new ArrayList<>());
        root.setOperator("and");

        var ff1 = new JPASearchInput.FilterSingleValue();
        ff1.setKey("stringOne");
        ff1.setValue("stringone");
        ff1.setOperator("startsWith");
        ff1.setOptions(new JPASearchInput.JPASearchFilterOptions());
        ff1.getOptions().setIgnoreCase(true);
        root.getFilters().add(ff1);

        var ff2 = new JPASearchInput.FilterSingleValue();
        ff2.setKey("stringTwo");
        ff2.setValue("two");
        ff2.setOperator("contains");
        ff2.setOptions(new JPASearchInput.JPASearchFilterOptions());
        ff2.getOptions().setIgnoreCase(true);
        root.getFilters().add(ff2);

        var ff3 = new JPASearchInput.FilterMultipleValues();
        ff3.setKey("stringMail");
        ff3.setValues(List.of("email1@example.com", "email2@example.com"));
        ff3.setOperator("in");
        root.getFilters().add(ff3);

        var ff4 = new JPASearchInput.RootFilter();
        ff4.setOperator("or");
        ff4.setFilters(new ArrayList<>());
        var ff41 = new JPASearchInput.FilterMultipleValues();
        ff41.setKey("stringFalse");
        ff41.setValues(List.of("StringFalse_1", "StringFalse_3"));
        ff41.setOperator("in");
        ff4.getFilters().add(ff41);
        var ff42 = new JPASearchInput.FilterSingleValue();
        ff42.setKey("stringFalse");
        ff42.setValue("StringFalse_2");
        ff42.setOperator("in");
        ff42.setOptions(new JPASearchInput.JPASearchFilterOptions());
        ff42.getOptions().setNegate(true);
        ff4.getFilters().add(ff42);
        root.getFilters().add(ff4);

        input.setFilter(root);
        List<MyEntity> res = myRepository.findAll(input, MyModel.class);
        assertNotNull(res);
        assertFalse(res.isEmpty());
        assertEquals(1, res.size());
        assertTrue(res.stream().anyMatch(r -> r.getId().equals(1L) && r.getEmail().equals("email1@example.com")));
    }

    private void setUp() {
        List<MyEntity> entities = new ArrayList<>();

        for (int i = 1; i <= 8; i++) {
            TestEntity5 testEntity5 = TestEntity5.builder()
                    .id((long) i)
                    .colTest5("Test5_" + i)
                    .build();

            TestEntity4 testEntity4 = TestEntity4.builder()
                    .id((long) i)
                    .colTest4("Test4_" + i)
                    .entity5(testEntity5)
                    .build();

            Set<TestEntity4> testEntity4Set = new HashSet<>();
            testEntity4Set.add(testEntity4);

            Set<TestEntity5> testEntity5Set = new HashSet<>();
            testEntity5Set.add(testEntity5);

            TestEntity1 testEntity1 = TestEntity1.builder()
                    .id((long) i)
                    .colTest1("Test1_" + i)
                    .build();

            TestEntity2 testEntity2 = TestEntity2.builder()
                    .id((long) i)
                    .colTest2("Test2_" + i)
                    .entities4(testEntity4Set)
                    .build();

            TestEntity3 testEntity3 = TestEntity3.builder()
                    .id((long) i)
                    .colTest3("Test3_" + i)
                    .entities5(testEntity5Set)
                    .build();

            MyEntity myEntity = MyEntity.builder()
                    .id((long) i)
                    .stringOne("StringOne_" + i)
                    .stringTwo("StringTwo_" + i)
                    .stringThree("StringThree_" + i)
                    .stringFalse("StringFalse_" + i)
                    .email("email" + i + "@example.com")
                    .primitiveInteger(i * 10)
                    .wrapperInteger(i * 20)
                    .stringDate(new Date())
                    .dateOne(new Date())
                    .primitiveLong(i * 100L)
                    .wrapperLongYes(i * 200L)
                    .primitiveFloat(i * 1.1f)
                    .wrapperFloat(i * 2.2f)
                    .primitiveDoubleYes(i * 3.3)
                    .wrapperDouble(i * 4.4)
                    .bigDecimal(BigDecimal.valueOf(i * 1000.0))
                    .localDateTime(LocalDateTime.now())
                    .localDate(LocalDate.now())
                    .localTime(LocalTime.now())
                    .offsetDateTime(OffsetDateTime.now())
                    .zonedDateTime(ZonedDateTime.now())
                    .offsetTime(OffsetTime.now())
                    .primitiveBoolean(i % 2 == 0)
                    .wrapperBoolean(i % 2 != 0)
                    .notSearchableOne("NotSearchableOne_" + i)
                    .notSearchableTwo("NotSearchableTwo_" + i)
                    .notSearchableThree(i * 300L)
                    .test1(testEntity1)
                    .test2(testEntity2)
                    .test3(testEntity3)
                    .build();

            entities.add(myEntity);
        }

        myRepository.saveAll(entities);
    }

}
