package app.tozzi.model;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Root;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class FieldRootBuilderBean<T> {

    public String field;
    public Root<T> root;
    public CriteriaBuilder criteriaBuilder;
    public Object value;
    public boolean trim;

}
