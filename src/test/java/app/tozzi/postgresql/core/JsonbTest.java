package app.tozzi.postgresql.core;

import app.tozzi.model.input.JPASearchInput;
import app.tozzi.postgresql.entity.MyPostgresEntity;
import app.tozzi.postgresql.model.MyPostgresModel;
import app.tozzi.postgresql.repository.MyPostgresRepository;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringBootConfiguration;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.data.jpa.test.autoconfigure.DataJpaTest;
import org.springframework.boot.jdbc.test.autoconfigure.AutoConfigureTestDatabase;
import org.springframework.boot.persistence.autoconfigure.EntityScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@DataJpaTest
@Testcontainers
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = JsonbTest.TestConfig.class)
public class JsonbTest {

    @SpringBootConfiguration
    @EnableAutoConfiguration
    @EntityScan("app.tozzi.postgresql.entity")
    @EnableJpaRepositories("app.tozzi.postgresql.repository")
    static class TestConfig {

    }

    @Container
    static PostgreSQLContainer<?> postgresContainer = new PostgreSQLContainer<>("postgres:15")
            .withDatabaseName("testdb")
            .withUsername("sa")
            .withPassword("sa");

    @DynamicPropertySource
    static void properties(DynamicPropertyRegistry registry) {
        registry.add("spring.datasource.url", postgresContainer::getJdbcUrl);
        registry.add("spring.datasource.username", postgresContainer::getUsername);
        registry.add("spring.datasource.password", postgresContainer::getPassword);
        registry.add("spring.jpa.hibernate.ddl-auto", () -> "update");
    }

    @Autowired
    private MyPostgresRepository repository;

    @BeforeEach
    void setup() {
        var e1 = new MyPostgresEntity();
        e1.setName("Biagio");
        e1.setData(Map.of("address", Map.of("street", "Via dei Test", "zipCode", "40100")));

        var e2 = new MyPostgresEntity();
        e2.setName("Tozzi");
        e2.setData(Map.of("address", Map.of("street", "Via dei Testoni", "zipCode", "80100")));

        repository.saveAll(List.of(e1, e2));
    }

    @AfterEach
    void after() {
        repository.deleteAll();
    }

    @AfterAll
    static void close() {
        postgresContainer.close();
    }

    @Test
    void test_1() {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setFilters(new ArrayList<>());
        root.setOperator("and");
        var ff1 = new JPASearchInput.FilterSingleValue();
        ff1.setKey("address");
        ff1.setValue("test");
        ff1.setOperator("contains");
        ff1.setOptions(new JPASearchInput.JPASearchFilterOptions());
        ff1.getOptions().setIgnoreCase(true);
        root.getFilters().add(ff1);
        input.setFilter(root);

        var res = repository.findAll(input, MyPostgresModel.class);
        assertNotNull(res);
        assertFalse(res.isEmpty());
        assertEquals(2, res.size());
    }

    @Test
    void test_2() {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setFilters(new ArrayList<>());
        root.setOperator("and");
        var ff1 = new JPASearchInput.FilterSingleValue();
        ff1.setKey("address");
        ff1.setValue("Testoni");
        ff1.setOperator("contains");
        ff1.setOptions(new JPASearchInput.JPASearchFilterOptions());
        ff1.getOptions().setIgnoreCase(false);
        root.getFilters().add(ff1);
        input.setFilter(root);

        var res = repository.findAll(input, MyPostgresModel.class);
        assertNotNull(res);
        assertFalse(res.isEmpty());
        assertEquals(1, res.size());
        assertEquals("Tozzi", res.get(0).getName());
    }

    @Test
    void test_3() {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setFilters(new ArrayList<>());
        root.setOperator("and");
        var ff1 = new JPASearchInput.FilterSingleValue();
        ff1.setKey("postalCode");
        ff1.setValue("00");
        ff1.setOperator("endsWith");
        ff1.setOptions(new JPASearchInput.JPASearchFilterOptions());
        ff1.getOptions().setIgnoreCase(false);
        root.getFilters().add(ff1);
        input.setFilter(root);

        var res = repository.findAll(input, MyPostgresModel.class);
        assertNotNull(res);
        assertFalse(res.isEmpty());
        assertEquals(2, res.size());
    }

    @Test
    void test_4() {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setFilters(new ArrayList<>());
        root.setOperator("and");
        var ff1 = new JPASearchInput.FilterSingleValue();
        ff1.setKey("postalCode");
        ff1.setValue("00");
        ff1.setOperator("endsWith");
        ff1.setOptions(new JPASearchInput.JPASearchFilterOptions());
        ff1.getOptions().setIgnoreCase(false);

        var ff2 = new JPASearchInput.FilterSingleValue();
        ff2.setKey("name");
        ff2.setValue("TOZZI");
        ff2.setOperator("eq");
        ff2.setOptions(new JPASearchInput.JPASearchFilterOptions());
        ff2.getOptions().setIgnoreCase(true);

        root.getFilters().add(ff1);
        root.getFilters().add(ff2);
        input.setFilter(root);

        var res = repository.findAll(input, MyPostgresModel.class);
        assertNotNull(res);
        assertFalse(res.isEmpty());
        assertEquals(1, res.size());
        assertEquals("Tozzi", res.get(0).getName());
    }

    @Test
    void test_eq_filter() {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setOperator("and");
        root.setFilters(new ArrayList<>());

        var f = new JPASearchInput.FilterSingleValue();
        f.setKey("name");
        f.setValue("Biagio");
        f.setOperator("eq");
        root.getFilters().add(f);

        input.setFilter(root);

        var res = repository.findAll(input, MyPostgresModel.class);
        assertEquals(1, res.size());
        assertEquals("Biagio", res.get(0).getName());
    }

    @Test
    void test_contains_filter_ignoreCase() {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setOperator("and");
        root.setFilters(new ArrayList<>());

        var f = new JPASearchInput.FilterSingleValue();
        f.setKey("address");
        f.setValue("test");
        f.setOperator("contains");
        f.setOptions(new JPASearchInput.JPASearchFilterOptions());
        f.getOptions().setIgnoreCase(true);

        root.getFilters().add(f);
        input.setFilter(root);

        var res = repository.findAll(input, MyPostgresModel.class);
        assertEquals(2, res.size());
    }

    @Test
    void test_in_filter() {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setOperator("and");
        root.setFilters(new ArrayList<>());

        var f = new JPASearchInput.FilterMultipleValues();
        f.setKey("name");
        f.setValues(List.of("Biagio", "Tozzi"));
        f.setOperator("in");

        root.getFilters().add(f);
        input.setFilter(root);

        var res = repository.findAll(input, MyPostgresModel.class);
        assertEquals(2, res.size());
    }

    @Test
    void test_startsWith_filter() {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setOperator("and");
        root.setFilters(new ArrayList<>());

        var f = new JPASearchInput.FilterSingleValue();
        f.setKey("name");
        f.setValue("Bi");
        f.setOperator("startsWith");

        root.getFilters().add(f);
        input.setFilter(root);

        var res = repository.findAll(input, MyPostgresModel.class);
        assertEquals(1, res.size());
        assertEquals("Biagio", res.get(0).getName());
    }

    @Test
    void test_endsWith_filter() {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setOperator("and");
        root.setFilters(new ArrayList<>());

        var f = new JPASearchInput.FilterSingleValue();
        f.setKey("postalCode");
        f.setValue("40100");
        f.setOperator("endsWith");

        root.getFilters().add(f);
        input.setFilter(root);

        var res = repository.findAll(input, MyPostgresModel.class);
        assertEquals(1, res.size());
        assertInstanceOf(Map.class, res.get(0).getData().get("address"));
        Map<String, Object> map = (Map<String, Object>) res.get(0).getData().get("address");
        assertEquals("40100", map.get("zipCode"));
    }

    @Test
    void test_gt_filter() {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setOperator("and");
        root.setFilters(new ArrayList<>());

        var f = new JPASearchInput.FilterSingleValue();
        f.setKey("id");
        f.setValue("0");
        f.setOperator("gt");

        root.getFilters().add(f);
        input.setFilter(root);

        var res = repository.findAll(input, MyPostgresModel.class);
        assertEquals(2, res.size());
    }

    @Test
    void test_gte_filter() {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setOperator("and");
        root.setFilters(new ArrayList<>());

        var f = new JPASearchInput.FilterSingleValue();
        f.setKey("id");
        f.setValue("2");
        f.setOperator("gte");

        root.getFilters().add(f);
        input.setFilter(root);

        var res = repository.findAll(input, MyPostgresModel.class);
        assertEquals(1, res.size());
        assertEquals("Tozzi", res.get(0).getName());
    }

    @Test
    void test_null_filter() {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setOperator("and");
        root.setFilters(new ArrayList<>());

        var f = new JPASearchInput.FilterSingleValue();
        f.setKey("address");
        f.setOperator("null");

        root.getFilters().add(f);
        input.setFilter(root);

        var res = repository.findAll(input, MyPostgresModel.class);
        assertEquals(0, res.size());
    }

    @Test
    void test_null_filter_2() {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setOperator("and");
        root.setFilters(new ArrayList<>());

        var f = new JPASearchInput.FilterSingleValue();
        f.setKey("address");
        f.setOperator("null");
        f.setOptions(new JPASearchInput.JPASearchFilterOptions());
        f.getOptions().setNegate(true);

        root.getFilters().add(f);
        input.setFilter(root);

        var res = repository.findAll(input, MyPostgresModel.class);
        assertEquals(2, res.size());
    }

    @Test
    void test_or_root_operator() {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setOperator("or");
        root.setFilters(new ArrayList<>());

        var f1 = new JPASearchInput.FilterSingleValue();
        f1.setKey("name");
        f1.setValue("Biagio");
        f1.setOperator("eq");

        var f2 = new JPASearchInput.FilterSingleValue();
        f2.setKey("postalCode");
        f2.setValue("80100");
        f2.setOperator("eq");

        root.getFilters().add(f1);
        root.getFilters().add(f2);
        input.setFilter(root);

        var res = repository.findAll(input, MyPostgresModel.class);
        assertEquals(2, res.size());
    }

    @Test
    void test_not_root_operator() {
        var input = new JPASearchInput();
        var root = new JPASearchInput.RootFilter();
        root.setOperator("not");
        root.setFilters(new ArrayList<>());

        var f = new JPASearchInput.FilterSingleValue();
        f.setKey("name");
        f.setValue("Biagio");
        f.setOperator("eq");

        root.getFilters().add(f);
        input.setFilter(root);

        var res = repository.findAll(input, MyPostgresModel.class);
        assertEquals(1, res.size());
        assertEquals("Tozzi", res.get(0).getName());
    }


}
