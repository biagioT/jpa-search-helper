package com.gisgro;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

@Repository
public interface TestEntity4Repository extends JpaRepository<TestEntity4, Long>, JpaSpecificationExecutor<TestEntity4> {
}
