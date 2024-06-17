package app.tozzi;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.SneakyThrows;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;


import java.math.BigDecimal;
import java.time.*;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;


@DataJpaTest
@ContextConfiguration(classes = {JpaTests.class})
@EnableAutoConfiguration
@TestPropertySource(properties = {
    "spring.jpa.show-sql=true",
    "logging.level.org.hibernate.SQL=DEBUG",
    "logging.level.org.hibernate.type.descriptor.sql.BasicBinder=TRACE"
})
public class JpaTests {
    ObjectMapper mapper = new ObjectMapper();
    @Autowired private TestEntityRepository testEntityRepository;
    @Autowired private TestEntity2Repository testEntity2Repository;

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
            ent2
        );
        testEntityRepository.save(ent);
    }

    @SneakyThrows
    @Test
    public void testAllFilters() {
        setup();
        var filterString = """
           ["and",
             ["and", 
                 ["and",
                   ["eq", ":primitiveInteger", 6],
                   ["iEq", ":email", "test@test.fi"],
                   ["lt", ":primitiveLong", 10],
                   ["in", ":primitiveDouble", 1.3, 1.4],
                   ["between", ":primitiveFloat", 1.3, 1.4],
                   ["lte", ":wrapperLong", 10],
                   ["nin", ":wrapperDouble", 1.3, 1.4],
                   ["isNull", ":wrapperInteger"],
                   ["eq", ":integerString", ""]
                ]
             ],
             ["and", 
               ["isNotNull", ":dateString"],
               ["notEq", ":bigDecimal",  ["bigDecimal", "1.35"]],
               ["eq", ":bigDecimal",  ["bigDecimal", "1.23"]],
               ["eq", ":nestedBean.string", "Nested! daa dumdidum"],
               ["iNotEq", ":nestedBean.string", "blaa!"],
               ["startsWith", ":nestedBean.string", "Nested!"],
               ["iStartsWith", ":nestedBean.string", "NESTED!"],
               ["contains", ":nestedBean.string", "Nested!"],
               ["iEndsWith", ":nestedBean.string", "DUM"],
               ["iContains", ":nestedBean.string", "NESTED!"],
               ["endsWith", ":nestedBean.string", "dum"]
             ]
          ]
           """;

        /*
             H2 does not support DATE function, so can't test this
             ["gt", ":date1", ["date", 2025, 1, 25]]
             ["gte", ":date2", ["date", 2024, 1, 25]]
        */

        JsonNode filters = mapper.readTree(filterString);
        List<TestEntity> result = testEntityRepository.findAll(filters, TestEntity.class);

        assertThat(result).hasSize(1);
    }

    @SneakyThrows
    @Test
    public void testOrOperator() {
        setup();
        var filterString = """
          ["or",
            ["eq", ":primitiveInteger", 6],
            ["eq", ":primitiveInteger", 7]
          ]
           """;

        JsonNode filters = mapper.readTree(filterString);
        List<TestEntity> result = testEntityRepository.findAll(filters, TestEntity.class);

        assertThat(result).hasSize(1);
    }

    @SneakyThrows
    @Test
    public void testNotOperator() {
        setup();
        var filterString = """
          ["not",
            ["eq", ":primitiveInteger", 7]
          ]
           """;

        JsonNode filters = mapper.readTree(filterString);
        List<TestEntity> result = testEntityRepository.findAll(filters, TestEntity.class);

        assertThat(result).hasSize(1);
    }
}