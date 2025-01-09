package app.tozzi.model;

import app.tozzi.annotation.Searchable;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = true)
public class ModelB extends ModelA {

    @Searchable
    private String modelBID;

    @Searchable
    private String modelBField;

}
