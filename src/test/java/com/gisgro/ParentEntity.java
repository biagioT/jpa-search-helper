package com.gisgro;

import com.gisgro.annotations.NestedSearchable;
import com.gisgro.annotations.Searchable;
import lombok.*;
import lombok.experimental.SuperBuilder;

import javax.persistence.*;

@Entity
@Getter
@Setter
@AllArgsConstructor
public class ParentEntity {
    public ParentEntity() {}

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Searchable
    private String parentField;
}
