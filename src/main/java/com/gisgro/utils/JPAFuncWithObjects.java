package com.gisgro.utils;

import com.gisgro.annotations.Searchable;
import org.apache.commons.lang3.tuple.Pair;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.Root;
import java.util.Map;

public interface JPAFuncWithObjects<V> {
    Expression<V> apply(Root<?> r, CriteriaQuery<?> query, CriteriaBuilder cb, Object[] u, Map<String, Pair<Searchable, Class<?>>> s);
}
