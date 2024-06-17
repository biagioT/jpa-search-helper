package app.tozzi.model;

import javax.persistence.criteria.CriteriaBuilder;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class FieldRootBuilderBean {
    public CriteriaBuilder cb;
    public Object[] values;
}
