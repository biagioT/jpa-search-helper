package app.tozzi.core;

import app.tozzi.annotation.Projectable;
import app.tozzi.entity.*;
import app.tozzi.model.MyModel;
import app.tozzi.model.input.JPASearchInput;
import app.tozzi.repository.MyRepository;
import app.tozzi.util.JPASearchUtils;
import app.tozzi.util.ReflectionUtils;
import jakarta.persistence.EntityManager;
import jakarta.persistence.Tuple;
import jakarta.persistence.TupleElement;
import jakarta.persistence.criteria.Selection;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.data.jpa.test.autoconfigure.DataJpaTest;
import org.springframework.boot.persistence.autoconfigure.EntityScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.time.*;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;

@DataJpaTest
@EnableAutoConfiguration
@ContextConfiguration(classes = {JPAProjectionProcessorTest.class})
@EntityScan("app.tozzi.entity")
@TestPropertySource(properties = {
        "spring.jpa.show-sql=true",
        "logging.level.org.hibernate.SQL=DEBUG",
        "logging.level.org.hibernate.type.descriptor.sql.BasicBinder=TRACE"
})
@EnableJpaRepositories("app.tozzi.repository")
public class JPAProjectionProcessorTest {

    @Autowired
    private EntityManager entityManager;

    @Autowired
    private MyRepository myRepository;

    @BeforeEach
    void init() {
        setUp();
    }

    @Test
    public void getQuery_mode2() {
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
        input.setOptions(new JPASearchInput.JPASearchOptions());
        input.getOptions().setSelections(List.of("stringMail", "mySubModel.searchMe"));
        var searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);
        var idFields = ReflectionUtils.getIdFields(MyEntity.class);
        var query = JPAProjectionProcessor.getQuery(input, MyModel.class, MyEntity.class, entityManager.getCriteriaBuilder(), idFields, false, null, null, searchableFields, false, null);

        assertEquals(4, query.getSelections().size());
    }

    @Test
    public void getQuery_mode1() {
        var searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);
        var idFields = ReflectionUtils.getIdFields(MyEntity.class);
        var input = JPASearchUtils.toObject(Map.of("id_eq", "1", "selections", "stringMail,mySubModel.searchMe"), false, false, true);
        var query = JPAProjectionProcessor.getQuery(input, MyModel.class, MyEntity.class, entityManager.getCriteriaBuilder(), idFields, false, null, null, searchableFields, false, null);

        assertEquals(4, query.getSelections().size());
    }

    @Test
    public void toMap_mocked() {
        var query = entityManager.getCriteriaBuilder().createTupleQuery();
        var root = query.from(MyEntity.class);

        var tuple1 = Mockito.mock(Tuple.class);
        when(tuple1.get("id")).thenReturn(1L);
        when(tuple1.get("email")).thenReturn("biagio.tozzi@gmail.com");
        when(tuple1.get("test1.entity6s.colTest6")).thenReturn("test_1");
        when(tuple1.get("test2.colTest2")).thenReturn("test_2");

        var tuple2 = Mockito.mock(Tuple.class);
        when(tuple2.get("id")).thenReturn(1L);
        when(tuple2.get("email")).thenReturn("biagio.tozzi@gmail.com");
        when(tuple2.get("test1.entity6s.colTest6")).thenReturn("test_1_2");
        when(tuple2.get("test2.colTest2")).thenReturn("test_2_2");

        var tuple3 = Mockito.mock(Tuple.class);
        when(tuple3.get("id")).thenReturn(2L);
        when(tuple3.get("email")).thenReturn("biagio.tozzi_2@gmail.com");
        when(tuple3.get("test1.entity6s.colTest6")).thenReturn("test_2");
        when(tuple3.get("test2.colTest2")).thenReturn("test_3");

        when(tuple1.get("test2.id")).thenReturn(1L);
        when(tuple2.get("test2.id")).thenReturn(2L);
        when(tuple3.get("test2.id")).thenReturn(3L);

        when(tuple1.get("test1.id")).thenReturn(1L);
        when(tuple2.get("test1.id")).thenReturn(1L);
        when(tuple3.get("test1.id")).thenReturn(3L);

        when(tuple1.get("test1.entity6s.id")).thenReturn(1L);
        when(tuple2.get("test1.entity6s.id")).thenReturn(2L);
        when(tuple3.get("test1.entity6s.id")).thenReturn(3L);


        List<TupleElement<?>> tupleElements = List.of(new TupleElement<>() {
            @Override
            public Class<?> getJavaType() {
                return null;
            }

            @Override
            public String getAlias() {
                return "email";
            }
        }, new TupleElement<>() {
            @Override
            public Class<?> getJavaType() {
                return null;
            }

            @Override
            public String getAlias() {
                return "test1.entity6s.colTest6";
            }
        }, new TupleElement<>() {
            @Override
            public Class<?> getJavaType() {
                return null;
            }

            @Override
            public String getAlias() {
                return "test2.colTest2";
            }
        }, new TupleElement<>() {
            @Override
            public Class<?> getJavaType() {
                return null;
            }

            @Override
            public String getAlias() {
                return "id";
            }
        }, new TupleElement<>() {
            @Override
            public Class<?> getJavaType() {
                return null;
            }

            @Override
            public String getAlias() {
                return "test1.id";
            }
        }, new TupleElement<>() {
            @Override
            public Class<?> getJavaType() {
                return null;
            }

            @Override
            public String getAlias() {
                return "test1.entity6s.id";
            }
        }, new TupleElement<>() {
            @Override
            public Class<?> getJavaType() {
                return null;
            }

            @Override
            public String getAlias() {
                return "test2.id";
            }
        });

        when(tuple1.getElements()).thenReturn(tupleElements);
        when(tuple2.getElements()).thenReturn(tupleElements);
        when(tuple3.getElements()).thenReturn(tupleElements);

        var selections = new ArrayList<Selection<?>>();
        selections.add(JPASearchUtils.getPath(root, "email").alias("email"));
        selections.add(JPASearchUtils.getPath(root, "test1.entity6s.colTest6").alias("test1.entity6s.colTest6"));
        selections.add(JPASearchUtils.getPath(root, "test2.colTest2").alias("test2.colTest2"));

        var list = JPAProjectionProcessor.toMap(List.of(tuple1, tuple2, tuple3), MyEntity.class, selections, ReflectionUtils.getIdFields(MyEntity.class));

        assertNotNull(list);
        assertEquals(2, list.size());

        var firstElement = list.get(0);
        assertTrue(firstElement.containsKey("email"));
        assertEquals(firstElement.get("email"), "biagio.tozzi@gmail.com");
        assertTrue(firstElement.containsKey("test1"));
        assertInstanceOf(Map.class, firstElement.get("test1"));
        var m1 = (Map) firstElement.get("test1");
        assertFalse(m1.isEmpty());
        assertTrue(m1.containsKey("entity6s"));
        assertInstanceOf(Set.class, m1.get("entity6s"));
        var s1 = (Set) m1.get("entity6s");
        assertFalse(s1.isEmpty());
        assertEquals(2, s1.size());
        var iterator = s1.iterator();
        var el1 = iterator.next();
        assertInstanceOf(Map.class, el1);
        var mel1 = (Map) el1;
        assertFalse(mel1.isEmpty());
        assertTrue(mel1.containsKey("colTest6"));
        assertEquals("test_1", mel1.get("colTest6"));
        var el2 = iterator.next();
        assertInstanceOf(Map.class, el2);
        var mel2 = (Map) el2;
        assertFalse(mel2.isEmpty());
        assertTrue(mel2.containsKey("colTest6"));
        assertEquals("test_1_2", mel2.get("colTest6"));

        var secondElement = list.get(1);
        assertTrue(secondElement.containsKey("email"));
        assertEquals(secondElement.get("email"), "biagio.tozzi_2@gmail.com");
        assertTrue(secondElement.containsKey("test1"));
        assertInstanceOf(Map.class, secondElement.get("test1"));
        var m3 = (Map) secondElement.get("test1");
        assertFalse(m3.isEmpty());
        assertTrue(m3.containsKey("entity6s"));
        assertInstanceOf(Set.class, m3.get("entity6s"));
        var s3 = (Set) m3.get("entity6s");
        assertFalse(s3.isEmpty());
        assertEquals(1, s3.size());
        var el = s3.iterator().next();
        assertInstanceOf(Map.class, el);
        var mel = (Map) el;
        assertFalse(mel.isEmpty());
        assertTrue(mel.containsKey("colTest6"));
        assertEquals("test_2", mel.get("colTest6"));
    }

    @Test
    public void toMap_withRealDatabaseTuples() {
        var rawSelections = List.of("stringMail", "mySubModel.searchMe", "list.other");
        var cb = entityManager.getCriteriaBuilder();
        var query = cb.createTupleQuery();
        var root = query.from(MyEntity.class);

        var selections = JPAProjectionProcessor.loadSelection(
                rawSelections,
                root,
                MyEntity.class,
                ReflectionUtils.getAllProjectableFields(MyModel.class),
                ReflectionUtils.getIdFields(MyEntity.class),
                false,
                false,
                null
        );

        query.multiselect(selections);
        var realTuples = entityManager.createQuery(query).getResultList();
        assertFalse(realTuples.isEmpty());

        var result = JPAProjectionProcessor.toMap(
                realTuples,
                MyEntity.class,
                selections,
                ReflectionUtils.getIdFields(MyEntity.class)
        );

        assertNotNull(result);
        assertEquals(8, result.size());

        var first = result.get(0);
        assertTrue(first.containsKey("email"));
        assertTrue(first.containsKey("test1"));
        assertTrue(first.containsKey("test2"));

        var test1 = (Map<String, Object>) first.get("test1");
        assertNotNull(test1);
        assertTrue(test1.containsKey("entity6s"));

        var entity6s = (Collection<?>) test1.get("entity6s");
        assertFalse(entity6s.isEmpty());

        var test2 = (Map<String, Object>) first.get("test2");
        assertNotNull(test2);
        assertTrue(test2.containsKey("colTest2"));
    }

    @Test
    public void toMap_handlesCartesianProduct() throws Exception {
        var complexEntity = new MyEntity();
        complexEntity.setId(999L);
        complexEntity.setEmail("complex@test.com");
        complexEntity.setKeywords(new ArrayList<>());
        complexEntity.getKeywords().add("K1");
        complexEntity.getKeywords().add("K2");

        var t1 = TestEntity1.builder().id(99L).colTest1("T1").entity6s(new HashSet<>()).build();

        var t6_1 = TestEntity6.builder().id(601L).colTest6("6-1").build();
        var t6_2 = TestEntity6.builder().id(602L).colTest6("6-2").build();
        entityManager.persist(t6_1);
        entityManager.persist(t6_2);

        t1.getEntity6s().add(t6_1);
        t1.getEntity6s().add(t6_2);
        complexEntity.setTest1(t1);

        myRepository.save(complexEntity);

        Map<String, Pair<Projectable, Field>> hackedProjectableFields = new HashMap<>(ReflectionUtils.getAllProjectableFields(MyModel.class));

        Projectable fakeProjectable = new Projectable() {
            @Override
            public Class<? extends Annotation> annotationType() {
                return Projectable.class;
            }

            @Override
            public String entityFieldKey() {
                return "";
            }
        };

        Field keywordsField = MyModel.class.getDeclaredField("keywords");
        hackedProjectableFields.put("keywords", Pair.of(fakeProjectable, keywordsField));

        var fields = List.of("keywords", "list.other");
        var cb = entityManager.getCriteriaBuilder();
        var cq = cb.createTupleQuery();
        var root = cq.from(MyEntity.class);
        cq.where(cb.equal(root.get("id"), 999L));

        var selections = JPAProjectionProcessor.loadSelection(
                fields, root, MyEntity.class,
                hackedProjectableFields,
                ReflectionUtils.getIdFields(MyEntity.class),
                false, false, null
        );
        cq.multiselect(selections);

        var tuples = entityManager.createQuery(cq).getResultList();
        assertTrue(tuples.size() >= 2);

        var result = JPAProjectionProcessor.toMap(
                tuples, MyEntity.class, selections, ReflectionUtils.getIdFields(MyEntity.class)
        );

        assertEquals(1, result.size());

        var rootEntity = result.get(0);

        var keywords = (Collection) rootEntity.get("keywords");
        assertNotNull(keywords, "Keywords non dovrebbe essere null grazie al trick!");
        assertEquals(2, keywords.size());

        var mapT1 = (Map) rootEntity.get("test1");
        var list6 = (Collection) mapT1.get("entity6s");
        assertEquals(2, list6.size());
    }

    @Test
    public void toMap_handlesNullRelationshipsGracefully() {
        var sparseEntity = new MyEntity();
        sparseEntity.setId(888L);
        sparseEntity.setEmail("nulls@test.com");
        myRepository.save(sparseEntity);

        var fields = List.of("stringMail", "list.other");
        var cb = entityManager.getCriteriaBuilder();
        var cq = cb.createTupleQuery();
        var root = cq.from(MyEntity.class);
        cq.where(cb.equal(root.get("id"), 888L));

        var selections = JPAProjectionProcessor.loadSelection(
                fields, root, MyEntity.class,
                ReflectionUtils.getAllProjectableFields(MyModel.class),
                ReflectionUtils.getIdFields(MyEntity.class),
                false, false, null
        );
        cq.multiselect(selections);

        var tuples = entityManager.createQuery(cq).getResultList();

        assertDoesNotThrow(() -> JPAProjectionProcessor.toMap(
                tuples, MyEntity.class, selections, ReflectionUtils.getIdFields(MyEntity.class)
        ));

        var result = JPAProjectionProcessor.toMap(
                tuples, MyEntity.class, selections, ReflectionUtils.getIdFields(MyEntity.class)
        );

        assertEquals(1, result.size());
        assertNull(result.get(0).get("test1"));
    }

    @Test
    public void loadSelection() {
        var query = entityManager.getCriteriaBuilder().createTupleQuery();
        var root = query.from(MyEntity.class);
        var selections = JPAProjectionProcessor.loadSelection(
                List.of("stringMail", "mySubModel.searchMe", "list.other"),
                root,
                MyEntity.class,
                ReflectionUtils.getAllProjectableFields(MyModel.class),
                ReflectionUtils.getIdFields(MyEntity.class),
                true, false, null
        );

        assertNotNull(selections);
        assertEquals(7, selections.size());
        assertTrue(selections.stream().anyMatch(s -> s.getAlias().equals("email")));
        assertTrue(selections.stream().anyMatch(s -> s.getAlias().equals("test2.colTest2")));
        assertTrue(selections.stream().anyMatch(s -> s.getAlias().equals("test1.entity6s.colTest6")));
    }

    private void setUp() {
        var entities = new ArrayList<MyEntity>();

        for (int i = 1; i <= 8; i++) {
            var testEntity5 = TestEntity5.builder()
                    .id((long) i)
                    .colTest5("Test5_" + i)
                    .build();

            var testEntity4 = TestEntity4.builder()
                    .id((long) i)
                    .colTest4("Test4_" + i)
                    .entity5(testEntity5)
                    .build();

            var testEntity4Set = new HashSet<TestEntity4>();
            testEntity4Set.add(testEntity4);

            var testEntity5Set = new HashSet<TestEntity5>();
            testEntity5Set.add(testEntity5);

            var testEntity6 = TestEntity6.builder()
                    .id((long) i)
                    .colTest6("Test6_" + i)
                    .build();

            entityManager.persist(testEntity6);

            var testEntity6Set = new HashSet<TestEntity6>();
            testEntity6Set.add(testEntity6);

            var testEntity1 = TestEntity1.builder()
                    .id((long) i)
                    .colTest1("Test1_" + i)
                    .entity6s(testEntity6Set)
                    .build();

            var testEntity2 = TestEntity2.builder()
                    .id((long) i)
                    .colTest2("Test2_" + i)
                    .entities4(testEntity4Set)
                    .build();

            var testEntity3 = TestEntity3.builder()
                    .id((long) i)
                    .colTest3("Test3_" + i)
                    .entities5(testEntity5Set)
                    .build();

            var myEntity = MyEntity.builder()
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
                    .keywords(new ArrayList<>(List.of("kw1_" + i)))
                    .build();

            entities.add(myEntity);
        }

        myRepository.saveAll(entities);
    }
}