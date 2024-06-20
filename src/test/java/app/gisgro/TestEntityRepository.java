package app.gisgro;

import app.gisgro.repository.JPASearchRepository;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TestEntityRepository extends JpaRepository<TestEntity, Long>, JPASearchRepository<TestEntity> {}
