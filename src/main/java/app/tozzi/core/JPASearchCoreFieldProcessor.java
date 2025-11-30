package app.tozzi.core;

import app.tozzi.annotation.Searchable;
import app.tozzi.exception.InvalidFieldException;
import app.tozzi.model.FieldDescriptor;
import app.tozzi.model.JPASearchType;
import org.apache.commons.lang3.tuple.Pair;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.stream.Stream;

public class JPASearchCoreFieldProcessor {

    protected static FieldDescriptor processField(String field, Map<String, String> entityFieldMap, Map<String, Pair<Searchable, Field>> searchableFields, boolean throwIfFieldNotExists, boolean throwsIfNotSortable, boolean checkSortable) {

        var tagMatch = searchableFields.values().stream()
                .filter(e -> e.getKey().tags() != null && e.getKey().tags().length > 0)
                .flatMap(e -> Stream.of(e.getKey().tags()).map(t -> Pair.of(e, t)))
                .filter(p -> p.getRight().fieldKey().equals(field))
                .findFirst()
                .orElse(null);

        if (!searchableFields.containsKey(field) && tagMatch == null) {
            if (throwIfFieldNotExists) {
                throw new InvalidFieldException("Field [" + field + "] does not exists or not sortable", field);
            }
            return null;
        }

        if (searchableFields.containsKey(field)) {
            var s = searchableFields.get(field).getKey();
            if (s.tags().length > 0 && Stream.of(s.tags()).noneMatch(t -> t.fieldKey().equals(field))) {
                throw new InvalidFieldException("Field [" + field + "] does not exists or not sortable", field);
            }
        }

        var searchable = searchableFields.containsKey(field) ? searchableFields.get(field).getKey() : tagMatch.getLeft().getKey();
        var type = searchableFields.containsKey(field) ? searchableFields.get(field).getValue().getType() : tagMatch.getLeft().getValue().getType();

        if (JPASearchType.JSONB.equals(searchable.targetType()) && (searchable.jsonPath() == null || searchable.jsonPath().isBlank())) {
            throw new InvalidFieldException("Invalid json path for field [" + field + "]", field);
        }

        if (checkSortable && !searchable.sortable()) {
            if (throwsIfNotSortable) {
                throw new InvalidFieldException("Field [" + field + "] is not sortable", field);
            }
            return null;
        }

        var tag = tagMatch != null ? tagMatch.getRight() : null;

        var entityField = entityFieldMap != null && entityFieldMap.containsKey(field) ? entityFieldMap.get(field) :
                (tag != null ?
                        (tag.entityFieldKey() != null && !tag.entityFieldKey().isBlank() ? tag.entityFieldKey() : field)
                        : (searchable.entityFieldKey() != null && !searchable.entityFieldKey().isBlank() ? searchable.entityFieldKey() : field));

        return new FieldDescriptor(field, searchable,
                JPASearchType.UNTYPED.equals(searchable.targetType()) ? JPASearchType.load(type, JPASearchType.STRING) : searchable.targetType(), entityField, type, JPASearchType.JSONB.equals(searchable.targetType()) ? searchable.jsonPath() : null);
    }
}