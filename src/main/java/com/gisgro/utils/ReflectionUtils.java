package com.gisgro.utils;

import com.gisgro.annotations.NestedSearchable;
import com.gisgro.annotations.Searchable;
import com.gisgro.exceptions.JPASearchException;
import org.apache.commons.lang3.reflect.FieldUtils;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.WildcardType;
import java.util.*;
import java.util.stream.Collectors;

public class ReflectionUtils {
    public static Map<String, List<Field>> getAllSearchableFields(Set<Class<?>> entityClasses) {
        return getAllSearchableFields(
                entityClasses,
                new ArrayList<>(),
                new HashMap<>(),
                true
        );
    }

    private static Map<String, List<Field>> getAllSearchableFields(
            Set<Class<?>> entityClasses,
            List<Field> path,
            Map<String, List<Field>> res,
            boolean evaluateNested
    ) {
        var fields = new HashSet<Field>();

        entityClasses.forEach(clazz -> {
            fields.addAll(Set.of(FieldUtils.getAllFields(clazz)));
        });

        fields.forEach(f -> {
            var newPath = new ArrayList<>(path);
            newPath.add(f);

            if (f.isAnnotationPresent(Searchable.class)) {
                res.putIfAbsent(
                        newPath
                                .stream()
                                .map(Field::getName)
                                .collect(Collectors.joining(".")),
                        newPath
                );
            }
            if (evaluateNested && f.isAnnotationPresent(NestedSearchable.class)) {
                var type = getType(f);
                getAllSearchableFields(
                        Set.of(type),
                        newPath,
                        res,
                        !entityClasses.contains(type)
                );
            }
        });

        return res;
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
