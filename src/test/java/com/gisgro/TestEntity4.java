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
public class TestEntity4 extends ParentEntity {
    public TestEntity4(
            Long id,
            String parentValue,
            String totallyNotPayload,
            TestEntity4 previous
    ) {
        super(id, parentValue);
        this.totallyNotPayload = totallyNotPayload;
        this.previous = previous;
    }

    @Searchable
    private String totallyNotPayload;

    @ManyToOne
    @NestedSearchable
    public TestEntity4 previous;
}
