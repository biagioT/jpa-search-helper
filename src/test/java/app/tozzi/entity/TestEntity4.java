package app.tozzi.entity;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.OneToOne;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Entity
public class TestEntity4 {

    @Id
    private Long id;

    private String colTest4;

    @OneToOne(cascade = CascadeType.ALL)
    private TestEntity5 entity5;

}
