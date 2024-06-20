package app.gisgro.utils;

import app.gisgro.annotations.Searchable;
import org.apache.commons.lang3.tuple.Pair;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Expression;
import java.util.Map;

public interface JPAFuncWithObjects<V> {
    Expression<V> apply(CriteriaBuilder t, Object[] u, Map<String, Pair<Searchable, Class<?>>> s);
}
