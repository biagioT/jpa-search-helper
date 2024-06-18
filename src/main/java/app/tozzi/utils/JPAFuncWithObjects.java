package app.tozzi.utils;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Expression;

public interface JPAFuncWithObjects<V> {
    Expression<V> apply(CriteriaBuilder t, Object[] u, Class<?> s);
}
