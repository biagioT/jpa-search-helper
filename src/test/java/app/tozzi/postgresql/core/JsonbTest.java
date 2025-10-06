package app.tozzi.postgresql.core;

import app.tozzi.model.input.JPASearchInput;
import app.tozzi.postgresql.entity.MyPostgresEntity;
import app.tozzi.postgresql.model.MyPostgresModel;
import app.tozzi.postgresql.repository.MyPostgresRepository;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.persistence.autoconfigure.EntityScan;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@Testcontainers
@SpringBootTest(classes = JsonbTest.TestApplication.class)
public class JsonbTest {

    @SpringBootApplication
    @EntityScan("app.tozzi.postgresql.entity")
    @EnableJpaRepositories("app.tozzi.postgresql.repository")
    static class TestApplication {
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
        e1.setData(Map.of("address", Map.of("street", "Via dei Test", "postalCode", "40100")));

        var e2 = new MyPostgresEntity();
        e2.setName("Tozzi");
        e2.setData(Map.of("address", Map.of("street", "Via dei Testoni", "postalCode", "80100")));

        repository.saveAll(List.of(e1, e2));
    }

    @AfterEach
    void after() {
        repository.deleteAll();
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
        root.getFilters().add(ff1);
        input.setFilter(root);

        var res = repository.findAll(input, MyPostgresModel.class);
        assertNotNull(res);
        assertFalse(res.isEmpty());
        assertEquals(1, res.size());
    }

}
