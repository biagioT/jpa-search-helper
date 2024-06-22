package app.tozzi.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.OneToOne;

@Entity
public class MyEntity2 {

    @Id
    private Long id;

    @OneToOne
    private TestEntity1 test1;

    @OneToOne
    private TestEntity2 test2;

    @OneToOne
    private TestEntity3 test3;

}
