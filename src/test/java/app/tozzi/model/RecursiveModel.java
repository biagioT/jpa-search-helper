package app.tozzi.model;

import app.tozzi.annotation.NestedSearchable;
import app.tozzi.annotation.Searchable;
import lombok.Data;

@Data
public class RecursiveModel {

    private Long id;

    @Searchable
    private String name;

    @NestedSearchable
    private RecursiveModel predecessor;

    @NestedSearchable
    private MyModel myModel;
}
