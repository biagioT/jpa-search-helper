package com.gisgro;

import com.gisgro.annotations.NestedSearchable;
import com.gisgro.annotations.Searchable;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;

@Entity
@Getter
@Setter
@NoArgsConstructor
public class TestEntity3 extends ParentEntity {
    public TestEntity3(
            Long id,
            String parentValue,
            String payload,
            TestEntity3 previous
    ) {
        super(id, parentValue);
        this.payload = payload;
        this.previous = previous;
    }

    @Searchable
    private String payload;

    @ManyToOne
    @NestedSearchable
    public TestEntity3 previous;
}
