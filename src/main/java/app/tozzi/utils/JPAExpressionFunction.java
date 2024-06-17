package app.tozzi.utils;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Expression;
import java.util.function.BiFunction;

public interface JPAExpressionFunction<T, V> extends BiFunction<CriteriaBuilder, Expression<T>[], Expression<V>> {}
