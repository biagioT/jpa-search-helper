package app.tozzi.utils;

import app.tozzi.exceptions.InvalidFieldException;
import org.springframework.beans.BeanUtils;

import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.util.stream.Stream;

public class ReflectionUtils {

    public static <T extends Annotation> boolean hasAnnotation(String field, PropertyDescriptor propertyDescriptor, Class<T> clazz) {

        try {
            return propertyDescriptor.getReadMethod().getDeclaringClass().getDeclaredField(field).isAnnotationPresent(clazz);

        } catch (NoSuchFieldException e) {
            throw new InvalidFieldException("Field [" + field + "] does not exists", e, field);
        }
    }

    public static <T extends Annotation> T getAnnotation(String field, PropertyDescriptor propertyDescriptor, Class<T> clazz) {

        try {
            return propertyDescriptor.getReadMethod().getDeclaringClass().getDeclaredField(field).getAnnotation(clazz);

        } catch (NoSuchFieldException e) {
            throw new InvalidFieldException("Field [" + field + "] does not exists", e, field);
        }
    }

    public static PropertyDescriptor getPropertyDescriptor(String fieldName, Class<?> clazz) {
        return Stream.of(BeanUtils.getPropertyDescriptors(clazz)).filter(p -> p.getName().equals(fieldName)).findAny().orElse(null);
    }

}
