package app.tozzi.core;

import app.tozzi.annotation.Searchable;
import app.tozzi.annotation.Tag;
import app.tozzi.exception.InvalidFieldException;
import app.tozzi.model.FieldDescriptor;
import app.tozzi.model.JPASearchType;
import org.apache.commons.lang3.tuple.Pair;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

public class JPASearchCoreFieldProcessor {

    protected static FieldDescriptor processField(String field, Map<String, String> entityFieldMap, Map<String, Pair<Searchable, Field>> searchableFields, boolean throwIfFieldNotExists, boolean throwsIfNotSortable, boolean checkSortable) {
        Map<String, Pair<Pair<Searchable, Field>, Tag>> tagMap = new HashMap<>();
        searchableFields.entrySet().stream().filter(e -> e.getValue().getKey().tags() != null && e.getValue().getKey().tags().length > 0)
                .forEach(e -> Stream.of(e.getValue().getKey().tags()).forEach(t -> {
                    tagMap.put(t.fieldKey(), Pair.of(e.getValue(), t));
                }));

        if (!searchableFields.containsKey(field) && !tagMap.containsKey(field)) {

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

        var searchable = searchableFields.containsKey(field) ? searchableFields.get(field).getKey() : tagMap.get(field).getKey().getKey();
        var type = searchableFields.containsKey(field) ? searchableFields.get(field).getValue().getType() : tagMap.get(field).getKey().getValue().getType();

        if (JPASearchType.JSONB.equals(searchable.targetType()) && (searchable.jsonPath() == null || searchable.jsonPath().isBlank())) {
            throw new InvalidFieldException("Invalid json path for field [" + field + "]", field);
        }

        if (checkSortable && !searchable.sortable()) {
            if (throwsIfNotSortable) {
                throw new InvalidFieldException("Field [" + field + "] is not sortable", field);
            }

            return null;
        }

        var entityField = entityFieldMap != null && entityFieldMap.containsKey(field) ? entityFieldMap.get(field) :
                (tagMap.containsKey(field) ?
                        (tagMap.get(field).getRight().entityFieldKey() != null && !tagMap.get(field).getRight().entityFieldKey().isBlank() ? tagMap.get(field).getRight().entityFieldKey() : field)
                        : (searchable.entityFieldKey() != null && !searchable.entityFieldKey().isBlank() ? searchable.entityFieldKey() : field));

        return new FieldDescriptor(field, searchable,
                JPASearchType.UNTYPED.equals(searchable.targetType()) ? JPASearchType.load(type, JPASearchType.STRING) : searchable.targetType(), entityField, type, JPASearchType.JSONB.equals(searchable.targetType()) ? searchable.jsonPath() : null);
    }
}
