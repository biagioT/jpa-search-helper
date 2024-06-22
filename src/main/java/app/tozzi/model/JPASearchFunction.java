package app.tozzi.model;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.Expression;

public interface JPASearchFunction<T, V> {

    Expression<V> apply(CriteriaBuilder t, Expression<T>[] expressions);

}
