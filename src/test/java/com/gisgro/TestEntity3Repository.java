package com.gisgro;

import com.gisgro.repository.JPASearchRepository;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TestEntity3Repository extends JpaRepository<TestEntity3, Long>, JPASearchRepository<TestEntity3> {}
