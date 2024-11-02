package app.tozzi.entity;

import jakarta.persistence.Embeddable;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import lombok.Data;

@Data
@Entity
public class MyEntity2 {

    @EmbeddedId
    private EmbeddedMyClass embeddedID;

    private Long longlong;

    @Data
    @Embeddable
    public static class EmbeddedMyClass {
        private String testId1;
        private String testId2;
    }

}
