package app.tozzi.repository;

import app.tozzi.entity.MyEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface MyRepository extends JpaRepository<MyEntity, Long>, JPASearchRepository<MyEntity> {

}
