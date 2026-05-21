package app.tozzi.core;

import app.tozzi.entity.MyEntity;
import app.tozzi.entity.TestEntity1;
import app.tozzi.entity.TestEntity2;
import app.tozzi.entity.TestEntity3;
import app.tozzi.model.MyModel;
import app.tozzi.model.input.JPASearchInput;
import app.tozzi.repository.MyRepository;
import app.tozzi.util.ReflectionUtils;
import jakarta.persistence.EntityManager;
import jakarta.persistence.Tuple;
import jakarta.persistence.criteria.JoinType;
import jakarta.persistence.criteria.Selection;
import org.apache.commons.lang3.reflect.FieldUtils;
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

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Coverage for the behavioural fixes introduced in release 4.0:
 * <ul>
 *     <li>#4 - DISTINCT is applied only when joins/fetches are actually used</li>
 *     <li>#6 - element collections support EMPTY/NULL via {@code cb.isEmpty}</li>
 *     <li>#14 - {@code JPAProjectionProcessor.toMap} no longer collapses entities whose compound
 *              keys are value-permutations of each other</li>
 *     <li>F  - NULL/EMPTY on a nested optional relationship correctly uses LEFT JOIN, so rows where
 *              the relation is null are not silently dropped</li>
 * </ul>
 */
@DataJpaTest
@EnableAutoConfiguration
@ContextConfiguration(classes = {JPASearchCoreFixes4Test.class})
@EntityScan("app.tozzi.entity")
@TestPropertySource(properties = {"spring.jpa.show-sql=true"})
@EnableJpaRepositories("app.tozzi.repository")
public class JPASearchCoreFixes4Test {

    @Autowired
    private MyRepository myRepository;

    @Autowired
    private EntityManager entityManager;

    @BeforeEach
    void seed() {
        // entity 1: has test1 (so the optional-relation tests can verify both branches)
        myRepository.save(MyEntity.builder()
                .id(1L)
                .email("a@example.com")
                .stringOne("alpha")
                .keywords(new ArrayList<>(List.of("a", "b")))
                .test1(TestEntity1.builder().id(1L).colTest1("t1-A").build())
                .test2(TestEntity2.builder().id(1L).colTest2("t2-A").build())
                .test3(TestEntity3.builder().id(1L).colTest3("t3-A").build())
                .build());

        // entity 2: NO test1 -> required to verify LEFT JOIN behaviour for IS NULL on nested path
        myRepository.save(MyEntity.builder()
                .id(2L)
                .email("b@example.com")
                .stringOne("beta")
                .keywords(new ArrayList<>())
                .test2(TestEntity2.builder().id(2L).colTest2("t2-B").build())
                .test3(TestEntity3.builder().id(2L).colTest3("t3-B").build())
                .build());
    }

    // ------------------------------------------------------------------------------------------------
    // #4 - DISTINCT applied conditionally
    // ------------------------------------------------------------------------------------------------

    @Test
    void distinct_notApplied_whenNoJoinsNorFetches() {
        var spec = JPASearchCore.<MyEntity>specification(
                singleFieldFilter("id", "1", "eq").getFilter(),
                ReflectionUtils.getAllSearchableFields(MyModel.class),
                null, null);

        var cb = entityManager.getCriteriaBuilder();
        var cq = cb.createQuery(MyEntity.class);
        var root = cq.from(MyEntity.class);
        cq.where(spec.toPredicate(root, cq, cb));

        assertFalse(cq.isDistinct(),
                "no joins nor fetches were required, DISTINCT must NOT be applied");
    }

    @Test
    void distinct_applied_whenFetchesArePresent() {
        var spec = JPASearchCore.<MyEntity>specification(
                singleFieldFilter("id", "1", "eq").getFilter(),
                ReflectionUtils.getAllSearchableFields(MyModel.class),
                Map.of("test1", JoinType.LEFT), null);

        var cb = entityManager.getCriteriaBuilder();
        var cq = cb.createQuery(MyEntity.class);
        var root = cq.from(MyEntity.class);
        cq.where(spec.toPredicate(root, cq, cb));

        assertTrue(cq.isDistinct(),
                "fetches were configured, DISTINCT must be applied to avoid cartesian-product duplicates");
    }

    @Test
    void distinct_applied_whenElementCollectionLeftJoinIsCreated() {
        // CONTAINS on an element collection forces a LEFT JOIN in JPASearchCore
        var spec = JPASearchCore.<MyEntity>specification(
                singleFieldFilter("keywords", "a", "contains").getFilter(),
                ReflectionUtils.getAllSearchableFields(MyModel.class),
                null, null);

        var cb = entityManager.getCriteriaBuilder();
        var cq = cb.createQuery(MyEntity.class);
        var root = cq.from(MyEntity.class);
        cq.where(spec.toPredicate(root, cq, cb));

        assertTrue(cq.isDistinct(),
                "element-collection LEFT JOIN was generated, DISTINCT must be applied");
    }

    // ------------------------------------------------------------------------------------------------
    // F - NULL / EMPTY on nested optional relationship: LEFT JOIN must be used
    // ------------------------------------------------------------------------------------------------

    @Test
    void nullFilter_onNestedOptionalRelationship_returnsRowsWhereRelationIsNull() {
        // searchMeAgain -> entityFieldKey "test1.colTest1": entity 2 has test1=null.
        // With an implicit inner join those rows would be silently dropped; the LEFT JOIN we now
        // build for NULL/EMPTY operators on nested paths fixes this.
        var input = singleFieldFilter("mySubModel.mySubSubModel.searchMeAgain", null, "null");

        var res = myRepository.findAll(input, MyModel.class);

        assertNotNull(res);
        assertEquals(1, res.size(), "only entity 2 (test1==null) must match");
        assertEquals(2L, res.get(0).getId());
    }

    @Test
    void notNullFilter_onNestedOptionalRelationship_returnsRowsWhereRelationIsNotNull() {
        // negate(NULL) -> "IS NOT NULL"; entity 1 has test1 populated -> matches; entity 2 -> no.
        var input = singleFieldFilter("mySubModel.mySubSubModel.searchMeAgain", null, "null");
        var fieldFilter = (JPASearchInput.FieldFilter) input.getFilter().getFilters().get(0);
        fieldFilter.setOptions(new JPASearchInput.JPASearchFilterOptions());
        fieldFilter.getOptions().setNegate(true);

        var res = myRepository.findAll(input, MyModel.class);

        assertNotNull(res);
        assertEquals(1, res.size(), "only entity 1 (test1 populated) must match");
        assertEquals(1L, res.get(0).getId());
    }

    // ------------------------------------------------------------------------------------------------
    // #6 - elementCollection + EMPTY
    // ------------------------------------------------------------------------------------------------

    @Test
    void emptyFilter_onElementCollection_matchesRowsWithEmptyCollection() {
        var input = singleFieldFilter("keywords", null, "empty");

        var res = myRepository.findAll(input, MyModel.class);

        assertNotNull(res);
        assertEquals(1, res.size(), "only entity 2 has an empty keywords collection");
        assertEquals(2L, res.get(0).getId());
    }

    @Test
    void emptyFilter_negated_onElementCollection_matchesRowsWithNonEmptyCollection() {
        var input = singleFieldFilter("keywords", null, "empty");
        var fieldFilter = (JPASearchInput.FieldFilter) input.getFilter().getFilters().get(0);
        fieldFilter.setOptions(new JPASearchInput.JPASearchFilterOptions());
        fieldFilter.getOptions().setNegate(true);

        var res = myRepository.findAll(input, MyModel.class);

        assertNotNull(res);
        assertEquals(1, res.size(), "only entity 1 has a non-empty keywords collection");
        assertEquals(1L, res.get(0).getId());
    }

    // ------------------------------------------------------------------------------------------------
    // #14 - toMap must not collapse distinct compound-id entities
    // ------------------------------------------------------------------------------------------------

    @Test
    void toMap_compoundIdPermutation_doesNotCollapseDistinctEntities() {
        // Two synthetic "compound id" entities whose key values are value-permutations of each
        // other: (id=1, primitiveInteger=2) and (id=2, primitiveInteger=1).
        // Pre-fix toMap sorted the id values by toString, so both rows hashed to the same CacheKey
        // and were merged into one result. Post-fix the ordering is keyed by field name, not by
        // value -> the two rows remain distinct.
        var idA = FieldUtils.getField(MyEntity.class, "id", true);
        var idB = FieldUtils.getField(MyEntity.class, "primitiveInteger", true);
        var idMap = new LinkedHashMap<String, Field>();
        idMap.put("id", idA);
        idMap.put("primitiveInteger", idB);
        Map<Class<?>, Map<String, Field>> idFields = Map.of(MyEntity.class, idMap);

        var t1 = Mockito.mock(Tuple.class);
        Mockito.when(t1.get("id")).thenReturn(1L);
        Mockito.when(t1.get("primitiveInteger")).thenReturn(2);
        Mockito.when(t1.get("email")).thenReturn("a@x");

        var t2 = Mockito.mock(Tuple.class);
        Mockito.when(t2.get("id")).thenReturn(2L);
        Mockito.when(t2.get("primitiveInteger")).thenReturn(1);
        Mockito.when(t2.get("email")).thenReturn("b@x");

        var root = entityManager.getCriteriaBuilder().createTupleQuery().from(MyEntity.class);
        List<Selection<?>> selections = List.of(
                root.get("id").alias("id"),
                root.get("primitiveInteger").alias("primitiveInteger"),
                root.get("email").alias("email"));

        var res = JPAProjectionProcessor.toMap(List.of(t1, t2), MyEntity.class, selections, idFields);

        assertEquals(2, res.size(),
                "distinct compound-id entities whose id values are permutations must NOT collapse");
        assertTrue(res.stream().anyMatch(m -> "a@x".equals(m.get("email"))));
        assertTrue(res.stream().anyMatch(m -> "b@x".equals(m.get("email"))));
    }

    // ------------------------------------------------------------------------------------------------
    // helpers
    // ------------------------------------------------------------------------------------------------

    private static JPASearchInput singleFieldFilter(String key, String value, String operator) {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setOperator("and");
        root.setFilters(new ArrayList<>());
        var f = new JPASearchInput.FilterSingleValue();
        f.setKey(key);
        f.setValue(value);
        f.setOperator(operator);
        root.getFilters().add(f);
        input.setFilter(root);
        return input;
    }
}


