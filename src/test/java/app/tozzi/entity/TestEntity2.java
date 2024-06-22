package app.tozzi.entity;

import jakarta.persistence.CascadeType;
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
public class TestEntity2 {

    @Id
    private Long id;

    private String colTest2;

    @OneToMany(cascade = CascadeType.ALL)
    private Set<TestEntity4> entities4;

}
