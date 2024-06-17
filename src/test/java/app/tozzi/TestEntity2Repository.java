package app.tozzi;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TestEntity2Repository extends JpaRepository<TestEntity2, Long> {}
