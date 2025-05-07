package com.gisgro;

import com.fasterxml.jackson.databind.JsonNode;
import com.gisgro.annotations.Searchable;
import com.gisgro.exceptions.InvalidFieldException;
import com.gisgro.exceptions.JPASearchException;
import com.gisgro.model.Operator;
import com.gisgro.model.SearchType;
import com.gisgro.utils.ReflectionUtils;
import lombok.AllArgsConstructor;
import lombok.Data;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.*;
import java.lang.reflect.Field;
import java.util.*;

import static com.gisgro.JPASearchFunctions.getPath;

public class JPASearchCore {
    public static <R, T> Specification<R> specification(
            JsonNode filterPayload,
            Class<T> entityClass,
            boolean throwsIfNotExistsOrNotSearchable
    ) {
        return specification(
                filterPayload,
                entityClass,
                throwsIfNotExistsOrNotSearchable,
                Collections.emptySet()
        );
    }

    public static <R, T> Specification<R> specification(
            JsonNode filterPayload,
            Class<T> entityClass,
            boolean throwsIfNotExistsOrNotSearchable,
            Set<Class<?>> searchableSubclasses
    ) {
        HashSet<Class<?>> entityClasses = new HashSet<>(searchableSubclasses);
        entityClasses.add(entityClass);

        var filterExpression = filterPayload.get("filter");

        return (root, query, criteriaBuilder) -> {
            var searchableFields = ReflectionUtils.getAllSearchableFields(entityClasses);
            var expr = processExpression(
                    filterExpression,
                    criteriaBuilder,
                    root,
                    query,
                    entityClasses,
                    throwsIfNotExistsOrNotSearchable,
                    searchableFields
            );
            if (expr instanceof Predicate) {
                return (Predicate) expr;
            } else {
                throw new JPASearchException("Not resulting a predicate" + expr);
            }
        };
    }

    private static <T> Object processValue(
            Operator op,
            JsonNode node,
            CriteriaBuilder cb,
            Root<?> root,
            CriteriaQuery<?> query,
            Set<Class<?>> entityClasses,
            boolean throwsIfNotExistsOrNotSearchable,
            Map<String, List<Field>> searchableFields
    ) {
        if (node.isTextual()) {
            var text = node.asText();
            if (Objects.equals(op.getName(), "field")) {
                return processField(
                        cb,
                        root,
                        throwsIfNotExistsOrNotSearchable,
                        searchableFields,
                        text
                );
            } else if (!op.isEvaluateStrings()) {
                return text;
            } else {
                return cb.literal(text);
            }
        } else if (node.isInt()) {
            return cb.literal(node.asInt());
        } else if (node.isLong()) {
            return cb.literal(node.asLong());
        } else if (node.isDouble()) {
            return cb.literal(node.asDouble());
        } else if (node.isBoolean()) {
            return cb.literal(node.asBoolean());
        } else if (node.isArray()) {
            return processExpression(node, cb, root, query, entityClasses, throwsIfNotExistsOrNotSearchable, searchableFields);
        } else {
            throw new JPASearchException("unexpected: " + node);
        }
    }

    private static <T, U extends T> Expression<?> processField(
            CriteriaBuilder cb,
            Root<T> root,
            boolean throwsIfNotExistsOrNotSearchable,
            Map<String, List<Field>> searchableFields,
            String text
    ) {
        var descriptor = loadDescriptor(
                text,
                throwsIfNotExistsOrNotSearchable,
                false,
                false,
                searchableFields
        );
        if (descriptor == null) {
            return null;
        }

        var path = getPath(cb, root, descriptor);
        var field = descriptor.fieldPath.get(descriptor.fieldPath.size() - 1);
        var searchable = field.getAnnotation(Searchable.class);

        if (searchable.trim() && descriptor.searchType == SearchType.STRING) {
            return cb.trim(path.as(String.class));
        } else if (field.getType().isEnum()) {
            return path.as(field.getType());
        }

        return path;
    }

    private static Expression<?> processExpression(
            JsonNode node,
            CriteriaBuilder cb,
            Root<?> root,
            CriteriaQuery<?> query,
            Set<Class<?>> entityClasses,
            boolean throwsIfNotExistsOrNotSearchable,
            Map<String, List<Field>> searchableFields
    ) {
        if (!node.isArray() || node.isEmpty() || !node.get(0).isTextual()) {
            throw new JPASearchException("Invalid expression");
        }

        var op = Operator.load(node.get(0).textValue());
        var arguments = new ArrayList<>();

        for (var i = 1; i < node.size(); i++) {
            var child = node.get(i);
            arguments.add(
                    processValue(
                            op,
                            child,
                            cb,
                            root,
                            query,
                            entityClasses,
                            throwsIfNotExistsOrNotSearchable,
                            searchableFields
                    )
            );
        }
        if (op.isEvaluateStrings()) {
            return op.getExprFunction().apply(cb, arguments.toArray(new Expression[0]));
        } else {
            return op.getObjFunction().apply(root, query, cb, arguments.toArray(), searchableFields);
        }
    }

    public static Sort loadSort(
            JsonNode filterPayload,
            Set<Class<?>> entityClasses,
            boolean throwsIfNotSortable,
            boolean throwsIfNotExistsOrNotSearchable
    ) {
        ArrayList<Sort.Order> orderSpecs = new ArrayList<>();
        var options = filterPayload.get("options");
        var searchableFields = ReflectionUtils.getAllSearchableFields(entityClasses);
        if (options != null) {
            var sortKeysNode = options.get("sortKey");
            if (sortKeysNode != null) {
                var keyList = new ArrayList<String>();
                if (sortKeysNode.isTextual()) {
                    keyList.add(sortKeysNode.asText());
                } else if (sortKeysNode.isArray()) {
                    for (var itm : sortKeysNode) {
                        keyList.add(itm.asText());
                    }
                }
                for (var sortKeyStr : keyList) {
                    var descending = false;
                    if (sortKeyStr.startsWith("-")) {
                        sortKeyStr = sortKeyStr.substring(1);
                        descending = true;
                    }

                    // noinspection unused: this is used for checking for sortability
                    var descriptor = loadDescriptor(
                            sortKeyStr,
                            throwsIfNotExistsOrNotSearchable,
                            true,
                            throwsIfNotSortable,
                            searchableFields
                    );

                    if (descending) {
                        orderSpecs.add(Sort.Order.desc(sortKeyStr));
                    } else {
                        orderSpecs.add(Sort.Order.asc(sortKeyStr));
                    }
                }
            }
        }
        return Sort.by(orderSpecs);
    }

    public static PageRequest loadSortAndPagination(
            JsonNode filterPayload,
            Class<?> entityClass,
            boolean throwsIfNotSortable,
            boolean throwsIfNotExistsOrSearchable
    ) {
        return loadSortAndPagination(
                filterPayload,
                entityClass,
                throwsIfNotSortable,
                throwsIfNotExistsOrSearchable,
                Collections.emptySet()
        );
    }

    public static PageRequest loadSortAndPagination(
            JsonNode filterPayload,
            Class<?> entityClass,
            boolean throwsIfNotSortable,
            boolean throwsIfNotExistsOrSearchable,
            Set<Class<?>> searchableSubclasses
    ) {
        HashSet<Class<?>> entityClasses = new HashSet<>(searchableSubclasses);
        entityClasses.add(entityClass);

        Integer pageSize = null;
        Integer pageOffset = null;
        Sort sort = null;

        var options = filterPayload.get("options");

        if (options != null) {
            sort = loadSort(
                    filterPayload,
                    entityClasses,
                    throwsIfNotSortable,
                    throwsIfNotExistsOrSearchable
            );

            var pageOffsetNode = options.get("pageOffset");
            if (pageOffsetNode != null) {
                pageOffset = pageOffsetNode.asInt();
            }
            var pageSizeNode = options.get("pageSize");
            if (pageSizeNode != null) {
                pageSize = pageSizeNode.asInt();
            }
        }

        if (pageSize == null) {
            throw new JPASearchException("Invalid or not present limit");
        }

        PageRequest result = PageRequest.ofSize(pageSize);
        if (pageOffset != null) {
            result = result.withPage(pageOffset);
        }
        if (sort != null) {
            result = result.withSort(sort);
        }

        return result;
    }

    public static Descriptor loadDescriptor(
            String key,
            boolean throwsIfNotExistsOrNotSortable,
            boolean checkSortable,
            boolean throwsIfNotSortable,
            Map<String, List<Field>> searchableFields
    ) {
        if (!searchableFields.containsKey(key)) {

            if (throwsIfNotExistsOrNotSortable) {
                throw new InvalidFieldException("Field [" + key + "] does not exists or not sortable", key);
            }

            return null;
        }

        var path = searchableFields.get(key);
        var field = path.get(path.size() - 1);
        var searchable = field.getAnnotation(Searchable.class);

        if (searchable == null) {
            return null;
        }

        if (checkSortable && !searchable.sortable()) {
            if (throwsIfNotSortable) {
                throw new InvalidFieldException("Field [" + key + "] is not sortable", key);
            }

            return null;
        }

        var searchType = SearchType.UNTYPED.equals(searchable.targetType())
                ? SearchType.load(field.getType(), SearchType.STRING)
                : searchable.targetType();

        return new Descriptor(searchType, path);
    }

    @Data
    @AllArgsConstructor
    public static class Descriptor {
        private SearchType searchType;
        private List<Field> fieldPath;
    }
}
