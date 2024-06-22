package app.tozzi.model;

import app.tozzi.annotation.Searchable;
import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class FieldDescriptor {

    private String path;
    private Searchable searchable;
    private JPASearchType JPASearchType;
    private String entityKey;

}
