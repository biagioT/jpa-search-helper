package com.gisgro;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

@Repository
public interface ParentEntityRepository extends JpaRepository<ParentEntity, Long>, JpaSpecificationExecutor<ParentEntity> {
}
