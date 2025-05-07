package com.gisgro.utils;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.Root;
import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;

public interface JPAFuncWithObjects<V> {
    Expression<V> apply(Root<?> r, CriteriaQuery<?> query, CriteriaBuilder cb, Object[] u, Map<String, List<Field>> s);
}
