package app.tozzi.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.OneToMany;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Set;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Entity
public class TestEntity1 {

    @Id
    private Long id;

    private String colTest1;

    @OneToMany
    private Set<TestEntity6> entity6s;

}
