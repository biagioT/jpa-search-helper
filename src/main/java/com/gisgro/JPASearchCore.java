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
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.*;
import java.util.ArrayList;
import java.util.Map;
import java.util.Objects;

import static com.gisgro.JPASearchFunctions.getPath;

public class JPASearchCore {
    public static <R, T> Specification<R> specification(JsonNode filterPayload,
                                                        Class<T> entityClass,
                                                        boolean throwsIfNotExistsOrNotSearchable) {

        var filterExpression = filterPayload.get("filter");

        return (root, query, criteriaBuilder) -> {
            var searchableFields = ReflectionUtils.getAllSearchableFields(entityClass);
            var expr = processExpression(
                    filterExpression,
                    criteriaBuilder,
                    root,
                    query,
                    entityClass,
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
            Class<T> entityClass,
            boolean throwsIfNotExistsOrNotSearchable,
            Map<String, Pair<Searchable, Class<?>>> searchableFields
    ) {
        if (node.isTextual()) {
            var text = node.asText();
            if (Objects.equals(op.getName(), "field")) {
                var descriptor = loadDescriptor(
                        text,
                        throwsIfNotExistsOrNotSearchable,
                        false,
                        false,
                        searchableFields
                );
                var path = getPath(root, text);
                if (descriptor.searchable.trim() && descriptor.searchType == SearchType.STRING) {
                    return cb.trim(path.as(String.class));
                } else if (descriptor.entityType.isEnum()) {
                    return path.as(descriptor.entityType);
                } else {
                    return path;
                }
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
            return processExpression(node, cb, root, query, entityClass, throwsIfNotExistsOrNotSearchable, searchableFields);
        } else if (node.isNull()) {
            return cb.nullLiteral(entityClass);
        } else {
            throw new JPASearchException("unexpected: " + node);
        }
    }

    private static Expression<?> processExpression(
            JsonNode node,
            CriteriaBuilder cb,
            Root<?> root,
            CriteriaQuery<?> query,
            Class<?> entityClass,
            boolean throwsIfNotExistsOrNotSearchable,
            Map<String, Pair<Searchable, Class<?>>> searchableFields
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
                            entityClass,
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
            Class<?> entityClass,
            boolean throwsIfNotSortable,
            boolean throwsIfNotExistsOrNotSearchable
    ) {
        ArrayList<Sort.Order> orderSpecs = new ArrayList<>();
        var options = filterPayload.get("options");
        var searchableFields = ReflectionUtils.getAllSearchableFields(entityClass);
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
                    var descriptor = loadDescriptor(
                            sortKeyStr,
                            throwsIfNotExistsOrNotSearchable,
                            true,
                            throwsIfNotSortable,
                            searchableFields
                    );

                    if (descending) {
                        orderSpecs.add(Sort.Order.desc(descriptor.entityKey));
                    } else {
                        orderSpecs.add(Sort.Order.asc(descriptor.entityKey));
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
        Integer pageSize = null;
        Integer pageOffset = null;
        Sort sort = null;

        var options = filterPayload.get("options");

        if (options != null) {
            sort = loadSort(
                    filterPayload,
                    entityClass,
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

    public static DescriptorBean loadDescriptor(String key,
                                                boolean throwsIfNotExistsOrNotSortable,
                                                boolean checkSortable,
                                                boolean throwsIfNotSortable,
                                                Map<String, Pair<Searchable, Class<?>>> searchableFields
    ) {
        if (!searchableFields.containsKey(key)) {

            if (throwsIfNotExistsOrNotSortable) {
                throw new InvalidFieldException("Field [" + key + "] does not exists or not sortable", key);
            }

            return null;
        }

        Searchable searchable = searchableFields.get(key).getKey();
        Class<?> type = searchableFields.get(key).getValue();

        if (checkSortable && !searchable.sortable()) {
            if (throwsIfNotSortable) {
                throw new InvalidFieldException("Field [" + key + "] is not sortable", key);
            }

            return null;
        }

        return new DescriptorBean(
                key, searchable,
                SearchType.UNTYPED.equals(searchable.targetType())
                        ? SearchType.load(type, SearchType.STRING)
                        : searchable.targetType(),
                key,
                type
        );
    }

    @Data
    @AllArgsConstructor
    public static class DescriptorBean {
        private String path;
        private Searchable searchable;
        private SearchType searchType;
        private String entityKey;
        private Class<?> entityType;
    }
}
