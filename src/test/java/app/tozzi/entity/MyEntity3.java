package app.tozzi.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.IdClass;
import lombok.Data;

@Entity
@IdClass(MyEntity3.IdClassMyEntity.class)
public class MyEntity3 {

    @Id
    private String testID1;

    @Id
    private String testID2;

    private Long longlong;

    @Data
    public static class IdClassMyEntity {
        private String testID1;
        private String testID2;
    }
}
