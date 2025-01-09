package app.tozzi.model;

import app.tozzi.annotation.Searchable;
import lombok.Data;

@Data
public class ModelA {

    @Searchable
    private String modelAID;

    @Searchable
    private String modelAField;

}
