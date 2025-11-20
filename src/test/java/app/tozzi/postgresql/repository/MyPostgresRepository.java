package app.tozzi.postgresql.repository;

import app.tozzi.postgresql.entity.MyPostgresEntity;
import app.tozzi.repository.JPASearchRepository;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface MyPostgresRepository extends JpaRepository<MyPostgresEntity, Long>, JPASearchRepository<MyPostgresEntity> {
}
