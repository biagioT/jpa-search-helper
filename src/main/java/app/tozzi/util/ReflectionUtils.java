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
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

public class ReflectionUtils {

    private static final ConcurrentHashMap<Class<?>, Map<String, Pair<Searchable, Field>>> SEARCHABLE_CACHE = new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Class<?>, Map<String, Pair<Projectable, Field>>> PROJECTABLE_CACHE = new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Class<?>, Map<Class<?>, Map<String, Field>>> ID_CACHE_BY_ROOT = new ConcurrentHashMap<>();

    public static Map<Class<?>, Map<String, Field>> getIdFields(Class<?> entityClass) {
        return ID_CACHE_BY_ROOT.computeIfAbsent(entityClass, ReflectionUtils::computeIdFieldsGraph);
    }

    private static Map<Class<?>, Map<String, Field>> computeIdFieldsGraph(Class<?> rootClass) {
        var resultMap = new HashMap<Class<?>, Map<String, Field>>();
        var visited = new HashSet<Class<?>>();
        computeIdFieldsRecursive("", rootClass, resultMap, visited);
        return resultMap;
    }

    private static void computeIdFieldsRecursive(String prefix, Class<?> currentClass, Map<Class<?>, Map<String, Field>> resultMap, Set<Class<?>> visited) {
        if (!visited.add(currentClass)) {
            return;
        }

        var localIdMap = new HashMap<String, Field>();
        var allFields = FieldUtils.getAllFieldsList(currentClass);

        var embeddedID = allFields.stream()
                .filter(f -> f.isAnnotationPresent(EmbeddedId.class))
                .findAny()
                .orElse(null);

        for (var f : allFields) {
            if (embeddedID != null && f.equals(embeddedID)) {
                var embeddableType = getType(embeddedID);
                for (var ef : FieldUtils.getAllFieldsList(embeddableType)) {
                    var path = prefix + embeddedID.getName() + "." + ef.getName();
                    localIdMap.put(path, ef);
                }
            } else if (embeddedID == null && f.isAnnotationPresent(Id.class)) {
                localIdMap.put(prefix + f.getName(), f);
            }
        }

        if (!localIdMap.isEmpty()) {
            resultMap.computeIfAbsent(currentClass, k -> new HashMap<>()).putAll(localIdMap);
        }

        for (var f : allFields) {
            if (isRelation(f)) {
                var targetType = getType(f);
                var newPrefix = prefix + f.getName() + ".";
                computeIdFieldsRecursive(newPrefix, targetType, resultMap, visited);
            }
        }
    }

    private static boolean isRelation(Field f) {
        return f.isAnnotationPresent(OneToMany.class) ||
                f.isAnnotationPresent(OneToOne.class) ||
                f.isAnnotationPresent(ManyToMany.class) ||
                f.isAnnotationPresent(ManyToOne.class);
    }

    public static Map<String, Pair<Searchable, Field>> getAllSearchableFields(Class<?> beanClass) {
        return SEARCHABLE_CACHE.computeIfAbsent(beanClass, key -> {
            var res = new HashMap<String, Pair<Searchable, Field>>();
            getFields("", beanClass, Searchable.class, NestedSearchable.class, res, true);
            return res;
        });
    }

    public static Map<String, Pair<Projectable, Field>> getAllProjectableFields(Class<?> beanClass) {
        return PROJECTABLE_CACHE.computeIfAbsent(beanClass, key -> {
            var res = new HashMap<String, Pair<Projectable, Field>>();
            getFields("", beanClass, Projectable.class, NestedProjectable.class, res, true);
            return res;
        });
    }

    private static <A extends Annotation, N extends Annotation> void getFields(String currentPath, Class<?> beanClass, Class<A> annotationClass, Class<N> nestedAnnotationClass, Map<String, Pair<A, Field>> res, boolean evaluateNested) {
        var fields = FieldUtils.getAllFieldsList(beanClass);

        for (var f : fields) {
            if (f.isAnnotationPresent(annotationClass)) {
                getType(f);

                var key = currentPath.isEmpty() ? f.getName() : currentPath + "." + f.getName();
                res.putIfAbsent(key, Pair.of(f.getAnnotation(annotationClass), f));
            }

            if (evaluateNested && f.isAnnotationPresent(nestedAnnotationClass)) {
                var type = getType(f);
                var nextPath = currentPath.isEmpty() ? f.getName() : currentPath + "." + f.getName();

                var shouldRecurseDeeper = evaluateNested && !type.equals(beanClass);

                getFields(nextPath, type, annotationClass, nestedAnnotationClass, res, shouldRecurseDeeper);
            }
        }
    }

    public static Class<?> getType(Field f) {
        var type = f.getType();

        if (Collection.class.isAssignableFrom(f.getType())) {
            var genericType = f.getGenericType();
            if (genericType instanceof ParameterizedType pt) {
                var arr = pt.getActualTypeArguments();
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
            }
            throw new JPASearchException("Invalid searchable type " + f.getType());
        }

        return type;
    }
}