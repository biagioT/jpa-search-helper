package app.gisgro.utils;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Expression;

public interface JPAFuncWithExpressions<T, V> {
    Expression<V> apply(CriteriaBuilder t, Expression<T>[] u);
}
