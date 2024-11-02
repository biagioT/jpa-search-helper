package app.tozzi.model;

import lombok.Data;

import java.util.LinkedList;

@Data
public class JPAEntityId {

    private LinkedList<Object> ids;

}
