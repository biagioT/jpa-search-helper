package app.tozzi.core;

import app.tozzi.annotation.Searchable;
import app.tozzi.exception.InvalidFieldException;
import app.tozzi.model.JPASearchOperatorFilter;
import app.tozzi.model.JPASearchType;
import app.tozzi.model.MyEnum;
import app.tozzi.util.ReflectionUtils;
import lombok.Data;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Demonstrates the current behaviour of the value processor when the searchable field is a
 * {@code Collection<EnumType>}: the resolved {@code Class<?>} in the FieldDescriptor is the raw
 * {@code List.class} (because {@link java.lang.reflect.Field#getType()} discards the generic argument),
 * therefore the enum-parsing routine fails because {@code List.class.isEnum() == false}.
 * <p>
 * This is the bug surfaced in the codebase review (item #7).
 */
public class JPASearchCoreValueProcessorCollectionEnumTest {

    @Data
    static class EnumCollectionModel {
        @Searchable(targetType = JPASearchType.ENUM)
        private List<MyEnum> tags;
    }

    @Test
    public void enumInCollection_currentlyFails_throwsInvalidFieldException() {
        var searchableFields = ReflectionUtils.getAllSearchableFields(EnumCollectionModel.class);
        var searchable = searchableFields.get("tags").getKey();
        var rawType = searchableFields.get("tags").getValue().getType();

        // Current behaviour: the raw field type is List.class (collection generic info is lost),
        // and parseEnum() refuses to operate on a non-enum class.
        assertEquals(List.class, rawType,
                "Field#getType() loses the collection's generic argument: the type stored is the raw collection class");

        var ex = assertThrows(InvalidFieldException.class, () -> JPASearchCoreValueProcessor.processValue(
                JPASearchOperatorFilter.EQ, JPASearchType.ENUM, searchable, "tags", "EN_VAL_1", rawType, false));

        assertTrue(ex.getMessage().contains("Invalid enum class"));
    }

    @Test
    public void enumInCollection_workaround_passingElementClassDirectly() {
        // If the caller manually passes the element class (MyEnum.class), parseEnum succeeds.
        // This shows the fix should be: resolve the collection's generic argument
        // (similarly to ReflectionUtils#getType) and store THAT in FieldDescriptor.type.
        var searchableFields = ReflectionUtils.getAllSearchableFields(EnumCollectionModel.class);
        var searchable = searchableFields.get("tags").getKey();

        var res = JPASearchCoreValueProcessor.processValue(
                JPASearchOperatorFilter.EQ, JPASearchType.ENUM, searchable, "tags", "EN_VAL_1", MyEnum.class, false);

        assertTrue(res.isPresent());
        assertEquals(MyEnum.EN_VAL_1, res.get());
    }
}

