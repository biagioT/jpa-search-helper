package app.tozzi.postgresql.model;

import app.tozzi.annotation.Searchable;
import app.tozzi.model.JPASearchType;
import lombok.Data;

@Data
public class MyPostgresModel {


    @Searchable
    private Long id;

    @Searchable
    private String name;

    @Searchable(targetType = JPASearchType.JSONB, entityFieldKey = "data", jsonPath = "address.street")
    private String address;

    @Searchable(targetType = JPASearchType.JSONB, entityFieldKey = "data", jsonPath = "address.zipCode")
    private String postalCode;

}
