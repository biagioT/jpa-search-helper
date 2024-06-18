package app.tozzi.utils;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Expression;

public interface JPAExpressionFunction2<V> {
    Expression<V> apply(CriteriaBuilder t, Object[] u, Class<?> s);
}
