package app.tozzi.util;

import app.tozzi.annotation.NestedProjectable;
import app.tozzi.annotation.NestedSearchable;
import app.tozzi.annotation.Projectable;
import app.tozzi.annotation.Searchable;
import app.tozzi.exception.JPASearchException;
import jakarta.persistence.*;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.commons.lang3.tuple.Pair;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.WildcardType;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class ReflectionUtils {

    private static final ConcurrentHashMap<Class<?>, Map<String, Pair<Searchable, Field>>> SEARCHABLE_CACHE = new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Class<?>, Map<String, Pair<Projectable, Field>>> PROJECTABLE_CACHE = new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Class<?>, Map<String, Field>> ID_CACHE = new ConcurrentHashMap<>();

    public static Map<Class<?>, Map<String, Field>> getIdFields(Class<?> entityClass) {
        return getIdFields(null, entityClass);
    }

    private static Map<Class<?>, Map<String, Field>> getIdFields(String prefix, Class<?> entityClass) {

        ID_CACHE.computeIfAbsent(entityClass, key -> {

            Field embeddedID = Stream.of(entityClass.getDeclaredFields())
                    .filter(f -> f.isAnnotationPresent(EmbeddedId.class)).findAny().orElse(null);

            return Stream.of(embeddedID != null ? getType(embeddedID).getDeclaredFields() : entityClass.getDeclaredFields()).filter(f -> embeddedID != null || f.isAnnotationPresent(Id.class))
                    .collect(Collectors.toMap(f -> (prefix != null ? prefix + "." : "") + (embeddedID != null ? embeddedID.getName() + "." + f.getName() : f.getName()), f -> f));
        });

        Stream.of(entityClass.getDeclaredFields())
                .filter(f -> f.isAnnotationPresent(OneToMany.class) || f.isAnnotationPresent(OneToOne.class) || f.isAnnotationPresent(ManyToMany.class) || f.isAnnotationPresent(ManyToOne.class))
                .filter(f -> !ID_CACHE.containsKey(getType(f)))
                .forEach(field -> getIdFields((prefix != null ? prefix + "." : "") + field.getName(), getType(field)));

        return ID_CACHE;

    }


    public static Map<String, Pair<Searchable, Field>> getAllSearchableFields(Class<?> beanClass) {

        return SEARCHABLE_CACHE.computeIfAbsent(beanClass, key -> {
            Map<String, Pair<Searchable, Field>> res = new HashMap<>();
            getFields(new StringBuilder(), beanClass, Searchable.class, NestedSearchable.class, res, true);
            return res;
        });

    }

    public static Map<String, Pair<Projectable, Field>> getAllProjectableFields(Class<?> beanClass) {

        return PROJECTABLE_CACHE.computeIfAbsent(beanClass, key -> {
            Map<String, Pair<Projectable, Field>> res = new HashMap<>();
            getFields(new StringBuilder(), beanClass, Projectable.class, NestedProjectable.class, res, true);
            return res;
        });

    }

    private static <A extends Annotation, N extends Annotation> void getFields(final StringBuilder root, Class<?> beanClass, Class<A> annotationClass, Class<N> nestedAnnotationClass, Map<String, Pair<A, Field>> res, boolean evaluateNested) {

        Stream.of(FieldUtils.getAllFields(beanClass))
                .forEach(f -> {

                    if (f.isAnnotationPresent(annotationClass)) {
                        res.putIfAbsent(root.isEmpty() ? f.getName() : root + "." + f.getName(), Pair.of(f.getAnnotation(annotationClass), f));
                    }

                    if (evaluateNested && f.isAnnotationPresent(nestedAnnotationClass)) {
                        if (!root.isEmpty()) {
                            root.append(".");
                        }
                        root.append(f.getName());
                        var type = getType(f);
                        getFields(root, type, annotationClass, nestedAnnotationClass, res, !type.equals(beanClass));

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
