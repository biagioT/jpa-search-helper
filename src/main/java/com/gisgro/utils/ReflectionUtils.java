package com.gisgro.utils;

import com.gisgro.annotations.NestedSearchable;
import com.gisgro.annotations.Searchable;
import com.gisgro.exceptions.JPASearchException;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.commons.lang3.tuple.Pair;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.WildcardType;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;

public class ReflectionUtils {
    private static final ConcurrentHashMap<Class<?>, Map<String, Pair<Searchable, Class<?>>>> cache = new ConcurrentHashMap<>();

    public static Map<String, Pair<Searchable, Class<?>>> getAllSearchableFields(Class<?> beanClass) {
        return cache.computeIfAbsent(beanClass, key -> {
            Map<String, Pair<Searchable, Class<?>>> res = new HashMap<>();
            getAllSearchableFields(new StringBuilder(), beanClass, res, true);
            return res;
        });
    }

    private static void getAllSearchableFields(
            final StringBuilder root,
            Class<?> beanClass,
            Map<String, Pair<Searchable, Class<?>>> res,
            boolean evaluateNested
    ) {
        Stream.of(FieldUtils.getAllFields(beanClass))
                .forEach(f -> {
                    if (f.isAnnotationPresent(Searchable.class)) {
                        res.putIfAbsent(root.isEmpty() ? f.getName() : root + "." + f.getName(), Pair.of(f.getAnnotation(Searchable.class), f.getType()));
                    }
                    if (evaluateNested && f.isAnnotationPresent(NestedSearchable.class)) {
                        if (!root.isEmpty()) {
                            root.append(".");
                        }
                        root.append(f.getName());

                        var type = getType(f);
                        getAllSearchableFields(root, type, res, !type.equals(beanClass));

                        if (root.indexOf(".") != -1) {
                            root.delete(root.lastIndexOf("."), root.length());

                        } else {
                            root.setLength(0);
                        }
                    }
                });
    }

    public static Class<?> getType(Field f) {
        Class<?> type = f.getType();

        if (Collection.class.isAssignableFrom(f.getType())) {
            var arr = ((ParameterizedType) f.getGenericType()).getActualTypeArguments();
            if (arr.length > 0) {
                if (arr[0] instanceof Class<?> clazz) {
                    return clazz;

                } else if (arr[0] instanceof WildcardType wt) {
                    if (wt.getLowerBounds().length > 0) {
                        return (Class<?>) wt.getLowerBounds()[0];
                    } else if (wt.getUpperBounds().length > 0) {
                        return (Class<?>) wt.getUpperBounds()[0];
                    }
                }
            }

            throw new JPASearchException("Invalid searchable type " + f.getType());
        }

        return type;
    }
}
