package app.tozzi.util;

import app.tozzi.annotation.NestedSearchable;
import app.tozzi.annotation.Searchable;
import app.tozzi.exception.JPASearchException;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.BeanUtils;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.WildcardType;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

public class ReflectionUtils {

    public static Map<String, Pair<Searchable, Class<?>>> getAllSearchableFields(Class<?> beanClass) {
        Map<String, Pair<Searchable, Class<?>>> res = new HashMap<>();
        getAllSearchableFields(new StringBuilder(), beanClass, res);
        return res;
    }

    private static void getAllSearchableFields(final StringBuilder root, Class<?> beanClass, Map<String, Pair<Searchable, Class<?>>> res) {

        Stream.of(BeanUtils.getPropertyDescriptors(beanClass)).flatMap(pd -> Stream.of(pd.getReadMethod().getDeclaringClass().getDeclaredFields()))
                .forEach(f -> {

                    if (f.isAnnotationPresent(Searchable.class)) {
                        res.putIfAbsent(root.isEmpty() ? f.getName() : root + "." + f.getName(), Pair.of(f.getAnnotation(Searchable.class), f.getType()));
                    }

                    if (f.isAnnotationPresent(NestedSearchable.class)) {
                        if (!root.isEmpty()) {
                            root.append(".");
                        }
                        root.append(f.getName());
                        getAllSearchableFields(root, getType(f), res);

                        if (root.indexOf(".") != -1) {
                            root.delete(root.lastIndexOf("."), root.length());

                        } else {
                            root.setLength(0);
                        }
                    }
                });

    }

    private static Class<?> getType(Field f) {
        Class<?> type = f.getType();

        if (Collection.class.isAssignableFrom(f.getType())) {
            var arr = ((ParameterizedType) f.getGenericType()).getActualTypeArguments();
            if (arr.length > 0) {
                if (arr[0] instanceof Class<?> clazz) {
                    return clazz;

                } else if (arr[0] instanceof WildcardType wt) {
                    if (wt.getLowerBounds() != null && wt.getLowerBounds().length > 0) {
                        return (Class<?>) wt.getLowerBounds()[0];

                    } else if (wt.getUpperBounds() != null && wt.getUpperBounds().length > 0) {
                        return (Class<?>) wt.getUpperBounds()[0];
                    }
                }
            }

            throw new JPASearchException("Invalid searchable type " + f.getType());
        }

        return type;
    }
}
