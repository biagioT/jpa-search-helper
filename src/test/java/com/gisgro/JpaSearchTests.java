package com.gisgro;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gisgro.model.Operator;
import com.gisgro.utils.JPAFuncWithObjects;
import lombok.SneakyThrows;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;

import javax.persistence.criteria.JoinType;
import java.math.BigDecimal;
import java.time.*;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;


@DataJpaTest
@ContextConfiguration(classes = {JpaSearchTests.class})
@EnableAutoConfiguration
@TestPropertySource(properties = {
    "spring.jpa.show-sql=true",
    "logging.level.org.hibernate.SQL=DEBUG",
    "logging.level.org.hibernate.type.descriptor.sql.BasicBinder=TRACE"
})
public class JpaSearchTests {
    ObjectMapper mapper = new ObjectMapper();
    @Autowired private TestEntityRepository testEntityRepository;
    @Autowired private TestEntity2Repository testEntity2Repository;
    @Autowired private TestEntity3Repository testEntity3Repository;

    private void setup() {
        var ent2 = new TestEntity2(
            0L,
            "Nested! daa dumdidum"
        );

        ent2 = testEntity2Repository.save(ent2);
        TestEntity ent = new TestEntity(
            0L,
            6,
            null,
            "asdf",
            "test@test.fi",
            "",
            "20240609",
            new Date(2024, Calendar.MARCH, 1),
            new Date(2024, Calendar.MARCH, 1),
            1L,
            10L,
            1.35F,
            5.6F,
            1.3,
            2.3,
            new BigDecimal("1.23"),
            LocalDateTime.now(),
            LocalDate.now(),
            LocalTime.now(),
            OffsetDateTime.now(),
            OffsetTime.now(),
            "fieldName",
            false,
            true,
            ent2,
            TestEnum.VALUE1,
            Period.parse("P6M")
        );
        testEntityRepository.save(ent);
    }

    private void setup2() {
        var ent2a = new TestEntity2(
            0L,
            "nested1"
        );
        ent2a = testEntity2Repository.save(ent2a);
        var ent2b = new TestEntity2(
            0L,
            "nested2"
        );
        ent2b = testEntity2Repository.save(ent2b);

        TestEntity ent = new TestEntity(
            0L,
            6,
            null,
            "asdf",
            "test@test.fi",
            "",
            "20240609",
            new Date(2024, Calendar.MARCH, 1),
            new Date(2024, Calendar.MARCH, 1),
            1L,
            10L,
            1.35F,
            5.6F,
            1.3,
            2.3,
            new BigDecimal("1.23"),
            LocalDateTime.now(),
            LocalDate.now(),
            LocalTime.now(),
            OffsetDateTime.now(),
            OffsetTime.now(),
            "fieldName",
            false,
            true,
            ent2a,
            TestEnum.VALUE1,
            Period.parse("P12M")
        );
        testEntityRepository.save(ent);
        ent = new TestEntity(
            0L,
            7,
            null,
            "asdf",
            "test@test.fi",
            "",
            "20240609",
            new Date(2024, Calendar.MARCH, 1),
            new Date(2024, Calendar.MARCH, 1),
            1L,
            10L,
            1.35F,
            5.6F,
            1.3,
            2.3,
            new BigDecimal("1.23"),
            LocalDateTime.now(),
            LocalDate.now(),
            LocalTime.now(),
            OffsetDateTime.now(),
            OffsetTime.now(),
            "fieldName",
            false,
            true,
            ent2b,
            TestEnum.VALUE2,
            Period.parse("P6M")
        );
        testEntityRepository.save(ent);
    }

    private void setup3() {
        var foo = testEntity3Repository.save(new TestEntity3(0L, "parentFoo", "foo", null));
        testEntity3Repository.save(new TestEntity3(0L, "parentBar", "bar", foo));
    }

    @SneakyThrows
    private <T> Specification<T> specificationFrom(String filterString, Class<T> clazz) {
        JsonNode filters = mapper.readTree(filterString);
        return JPASearchCore.specification(filters, clazz, true);
    }

    @SneakyThrows
    private <T> PageRequest pageRequestFrom(String filterString, Class<T> clazz) {
        JsonNode filters = mapper.readTree(filterString);
        return JPASearchCore.loadSortAndPagination(filters, clazz, true, true);
    }

    @Test
    public void testLinked() {
        setup3();

        var filterString = """
            {"filter":
                ["and",
                 ["eq", ["field", "parentField"], "parentBar"],
                 ["eq", ["field", "payload"], "bar"],
                 ["eq", ["field", "previous.payload"], "foo"]
                ]}
        """;

        List<TestEntity3> result = testEntity3Repository.findAll(specificationFrom(filterString, TestEntity3.class));
        assertThat(result).hasSize(1);
    }

    @Test
    public void testAllFilters() {
        setup();
        var filterString = """
          {
            "filter":
    ["and",
      ["and",
        ["and",
          ["eq", ["field","primitiveInteger"], 6],
          ["eq", ["lower" , ["field","email"]], "test@test.fi"],
          ["lt", ["field" ,"primitiveLong"], 10],
          ["in", ["field","primitiveDouble"], 1.3, 1.4],
          ["between", ["field","primitiveFloat"], 1.3, 1.4],
          ["lte", ["field","wrapperLong"], 10],
          ["not", ["in", ["field","wrapperDouble"], 1.3, 1.4]],
          ["isNull", ["field","wrapperInteger"]],
          ["eq", ["field","integerString"], ""],
          ["eq", ["field","testEnum"], ["enum", "TestEnum", "VALUE1"]],
          ["eq", ["field","period"], ["period", "P6M"]]
        ]
      ],
      ["and",
        ["not", ["isNull", ["field" ,"dateString"]]],
        ["not", ["eq", ["field","bigDecimal"],  ["bigDecimal", "1.35"]]],
        ["eq", ["field","bigDecimal"],  ["bigDecimal", "1.23"]],
        ["eq", ["field","nestedBean.string"], "Nested! daa dumdidum"],
        ["not", ["eq", ["lower", ["field","nestedBean.string"]], "blaa!"]],
        ["startsWith", ["field","nestedBean.string"], "Nested!"],
        ["startsWith", ["lower" ,["field","nestedBean.string"]], "nested!"],
        ["contains", ["field","nestedBean.string"], "Nested!"],
        ["contains", ["lower",["field","nestedBean.string"]], "nested!"],
        ["endsWith", ["field","nestedBean.string"], "dum"],
        ["endsWith", ["lower",["field","nestedBean.string"]], "dum"]
      ]
      ]            
   
   }
          """;

        /*
             H2 does not support STR_TO_DATE function, so can't test this

        ["gt", "date1", ["date", "2018-04-26T15:41:49Z"]],
        ["gte", "date2", ["date", "2018-04-26T15:41:49Z"]],

        */

        List<TestEntity> result = testEntityRepository.findAll(specificationFrom(filterString, TestEntity.class));

        assertThat(result).hasSize(1);
    }

    @Test
    public void testOrOperator() {
        setup();
        var filterString = """
          {
           "filter": ["or",
            ["eq", ["field", "primitiveInteger"], 6],
            ["eq", ["field", "primitiveInteger"], 7]
           ]
          }
          """;

        List<TestEntity> result = testEntityRepository.findAll(specificationFrom(filterString, TestEntity.class));

        assertThat(result).hasSize(1);
    }

    @Test
    public void testNotOperator() {
        setup();
        var filterString = """
          {"filter": ["not",
            ["eq", ["field", "primitiveInteger"], 7]
          ]}
          """;

        List<TestEntity> result = testEntityRepository.findAll(specificationFrom(filterString, TestEntity.class));

        assertThat(result).hasSize(1);
    }

    @Test
    public void testEnum() {
        setup2();
        var filterString = """
          {"filter": ["eq", ["field", "testEnum"], ["enum", "TestEnum", "VALUE1"]]}
          """;

        List<TestEntity> result = testEntityRepository.findAll(specificationFrom(filterString, TestEntity.class));

        assertThat(result).hasSize(1);
    }

    @Test
    public void testSort() {
        setup2();
        var filterString = """
          {"filter": ["gte", ["field", "primitiveInteger"], 6],
          "options": {
            "sortKey": ["primitiveInteger"],
            "pageSize": 2,
            "pageOffset": 0
          }
          }
          """;

        Page<TestEntity> result = testEntityRepository.findAll(
                specificationFrom(filterString, TestEntity.class),
                pageRequestFrom(filterString, TestEntity.class)
        );
        assertThat(result).hasSize(2);
        assertThat(result.getTotalPages()).isEqualTo(1);
        assertThat(result.getContent().size()).isEqualTo(2);
        assertThat(result.getContent().get(0).getPrimitiveInteger()).isEqualTo(6);
        assertThat(result.getContent().get(1).getPrimitiveInteger()).isEqualTo(7);
    }

    @Test
    public void testSortDesc() {
        setup2();
        var filterString = """
          {"filter": ["gte", ["field", "primitiveInteger"], 6],
          "options": {
            "sortKey": ["-primitiveInteger"],
            "pageSize": 2,
            "pageOffset": 0
          }
          }
          """;

        Page<TestEntity> result = testEntityRepository.findAll(
                specificationFrom(filterString, TestEntity.class),
                pageRequestFrom(filterString, TestEntity.class)
        );
        assertThat(result).hasSize(2);
        assertThat(result.getTotalPages()).isEqualTo(1);
        assertThat(result.getContent().size()).isEqualTo(2);
        assertThat(result.getContent().get(0).getPrimitiveInteger()).isEqualTo(7);
        assertThat(result.getContent().get(1).getPrimitiveInteger()).isEqualTo(6);
    }

    @Test
    public void testSortDesc2() {
        setup2();
        var filterString = """
          {"filter": ["gte", ["field", "primitiveInteger"], 6],
          "options": {
            "sortKey": "-primitiveInteger",
            "pageSize": 2,
            "pageOffset": 0
          }
          }
          """;

        Page<TestEntity> result = testEntityRepository.findAll(
                specificationFrom(filterString, TestEntity.class),
                pageRequestFrom(filterString, TestEntity.class)
        );
        assertThat(result).hasSize(2);
        assertThat(result.getTotalPages()).isEqualTo(1);
        assertThat(result.getContent().size()).isEqualTo(2);
        assertThat(result.getContent().get(0).getPrimitiveInteger()).isEqualTo(7);
        assertThat(result.getContent().get(1).getPrimitiveInteger()).isEqualTo(6);
    }

    @Test
    public void testSortLimit() {
        setup2();
        var filterString = """
          {"filter": ["gte", ["field", "primitiveInteger"], 6],
          "options": {
            "sortKey": ["primitiveInteger"],
            "pageSize": 1,
            "pageOffset": 0
          }
          }
          """;

        Page<TestEntity> result = testEntityRepository.findAll(
                specificationFrom(filterString, TestEntity.class),
                pageRequestFrom(filterString, TestEntity.class)
        );
        assertThat(result.getTotalPages()).isEqualTo(2);
        assertThat(result.getContent().size()).isEqualTo(1);
        assertThat(result.getContent().get(0).getPrimitiveInteger()).isEqualTo(6);
    }

    @Test
    public void testSortLimit2() {
        setup2();
        var filterString = """
          {"filter": ["gte", ["field", "primitiveInteger"], 6],
          "options": {
            "sortKey": ["primitiveInteger"],
            "pageSize": 1,
            "pageOffset": 1
          }
          }
          """;

        Page<TestEntity> result = testEntityRepository.findAll(
                specificationFrom(filterString, TestEntity.class),
                pageRequestFrom(filterString, TestEntity.class)
        );
        assertThat(result.getTotalPages()).isEqualTo(2);
        assertThat(result.getContent().size()).isEqualTo(1);
        assertThat(result.getContent().get(0).getPrimitiveInteger()).isEqualTo(7);
    }

    @Test
    public void testNestedSortLimit() {
        setup2();
        var filterString = """
          {"filter": ["gte", ["field", "primitiveInteger"], 6],
          "options": {
            "sortKey": ["nestedBean.string"],
            "pageSize": 1,
            "pageOffset": 1
          }
          }
          """;

        Page<TestEntity> result = testEntityRepository.findAll(
                specificationFrom(filterString, TestEntity.class),
                pageRequestFrom(filterString, TestEntity.class)
        );
        assertThat(result.getTotalPages()).isEqualTo(2);
        assertThat(result.getContent().size()).isEqualTo(1);
        assertThat(result.getContent().get(0).getPrimitiveInteger()).isEqualTo(7);
    }

    @Test
    public void testNestedSortLimitDesc() {
        setup2();
        var filterString = """
          {"filter": ["gte", ["field", "primitiveInteger"], 6],
          "options": {
            "sortKey": ["-nestedBean.string"],
            "pageSize": 1,
            "pageOffset": 1
          }
          }
          """;

        Page<TestEntity> result = testEntityRepository.findAll(
                specificationFrom(filterString, TestEntity.class),
                pageRequestFrom(filterString, TestEntity.class)
        );
        assertThat(result.getTotalPages()).isEqualTo(2);
        assertThat(result.getContent().size()).isEqualTo(1);
        assertThat(result.getContent().get(0).getPrimitiveInteger()).isEqualTo(6);
    }

    @Test
    public void testNestedSortLimitMultipleCriteria() {
        setup2();
        var filterString = """
          {"filter": ["gte", ["field", "primitiveInteger"], 6],
          "options": {
            "sortKey": ["primitiveInteger", "-nestedBean.string"],
            "pageSize": 1,
            "pageOffset": 1
          }
          }
          """;

        Page<TestEntity> result = testEntityRepository.findAll(
                specificationFrom(filterString, TestEntity.class),
                pageRequestFrom(filterString, TestEntity.class)
        );
        assertThat(result.getTotalPages()).isEqualTo(2);
        assertThat(result.getContent().size()).isEqualTo(1);
        assertThat(result.getContent().get(0).getPrimitiveInteger()).isEqualTo(7);
    }

    @Test
    public void testOwnBooleanOperator() {
        setup2();

        JPAFuncWithObjects<Boolean> func = (root, query, cb, values, searchableFields) -> {
            var nestedBean = root.join("nestedBean", JoinType.LEFT);
            return cb.equal(nestedBean.get("string"), values[0]);
        };

        Operator.addOperator(new Operator("ownFunc", func));

        var filterString = """
          {"filter": ["ownFunc", "nested2"]}
          """;

        List<TestEntity> result = testEntityRepository.findAll(specificationFrom(filterString, TestEntity.class));
        assertThat(result.get(0).getPrimitiveInteger()).isEqualTo(7);
    }

    @Test
    public void testOwnStringOperator() {
        setup2();

        JPAFuncWithObjects<String> func = (root, query, cb, values, searchableFields) -> {
            var nestedBean = root.join("nestedBean", JoinType.LEFT);
            return cb.concat(nestedBean.get("string"), cb.literal((String)values[0]));
        };

        Operator.addOperator(new Operator("ownFunc2", func));

        var filterString = """
          {"filter": ["eq", ["ownFunc2", "blah"], "nested2blah"]}
          """;

        List<TestEntity> result = testEntityRepository.findAll(specificationFrom(filterString, TestEntity.class));
        assertThat(result.get(0).getPrimitiveInteger()).isEqualTo(7);
    }
}