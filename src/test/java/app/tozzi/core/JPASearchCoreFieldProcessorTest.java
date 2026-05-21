 package app.tozzi.core;

import app.tozzi.exception.InvalidFieldException;
import app.tozzi.model.JPASearchType;
import app.tozzi.model.MyEnum;
import app.tozzi.model.MyModel;
import app.tozzi.util.ReflectionUtils;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

public class JPASearchCoreFieldProcessorTest {

    @Test
    public void processField() {
        var searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);

        var fd = JPASearchCoreFieldProcessor.processField("stringOne", Collections.emptyMap(), searchableFields, true, false, false);
        assertNotNull(fd);
        assertEquals("stringOne", fd.getPath());
        assertEquals("stringOne", fd.getEntityKey());
        assertEquals(JPASearchType.STRING, fd.getSearchType());
        assertNotNull(fd.getSearchable());

        fd = JPASearchCoreFieldProcessor.processField("stringOne", Map.of("stringOne", "stringOneReplaced"), searchableFields, true, false, false);
        assertNotNull(fd);
        assertEquals("stringOne", fd.getPath());
        assertEquals("stringOneReplaced", fd.getEntityKey());
        assertEquals(JPASearchType.STRING, fd.getSearchType());
        assertNotNull(fd.getSearchable());

        fd = JPASearchCoreFieldProcessor.processField("id", Collections.emptyMap(), searchableFields, true, false, false);
        assertNotNull(fd);
        assertEquals("id", fd.getPath());
        assertEquals("id", fd.getEntityKey());
        assertEquals(JPASearchType.LONG, fd.getSearchType());
        assertNotNull(fd.getSearchable());

        fd = JPASearchCoreFieldProcessor.processField("primitiveDouble", Collections.emptyMap(), searchableFields, true, false, false);
        assertNotNull(fd);
        assertEquals("primitiveDouble", fd.getPath());
        assertEquals("primitiveDoubleYes", fd.getEntityKey());
        assertEquals(JPASearchType.DOUBLE, fd.getSearchType());
        assertNotNull(fd.getSearchable());

        assertThrows(InvalidFieldException.class, () -> JPASearchCoreFieldProcessor.processField("wrapperLong", Collections.emptyMap(), searchableFields, true, false, false));

        fd = JPASearchCoreFieldProcessor.processField("wrapperLong.one", Collections.emptyMap(), searchableFields, true, false, false);
        assertNotNull(fd);
        assertEquals("wrapperLong.one", fd.getPath());
        assertEquals("wrapperLong.one", fd.getEntityKey());
        assertEquals(JPASearchType.LONG, fd.getSearchType());
        assertNotNull(fd.getSearchable());

        fd = JPASearchCoreFieldProcessor.processField("wrapperLong.two", Collections.emptyMap(), searchableFields, true, false, false);
        assertNotNull(fd);
        assertEquals("wrapperLong.two", fd.getPath());
        assertEquals("wrapperLongYes", fd.getEntityKey());
        assertEquals(JPASearchType.LONG, fd.getSearchType());
        assertNotNull(fd.getSearchable());

        fd = JPASearchCoreFieldProcessor.processField("wrapperLong.two", Map.of("wrapperLong.two", "wrapperLong.replaced"), searchableFields, true, false, false);
        assertNotNull(fd);
        assertEquals("wrapperLong.two", fd.getPath());
        assertEquals("wrapperLong.replaced", fd.getEntityKey());
        assertEquals(JPASearchType.LONG, fd.getSearchType());
        assertNotNull(fd.getSearchable());

        fd = JPASearchCoreFieldProcessor.processField("mySubModel.mySubSubModel.searchMeAgain", Collections.emptyMap(), searchableFields, true, false, false);
        assertNotNull(fd);
        assertEquals("mySubModel.mySubSubModel.searchMeAgain", fd.getPath());
        assertEquals("test1.colTest1", fd.getEntityKey());
        assertEquals(JPASearchType.STRING, fd.getSearchType());
        assertNotNull(fd.getSearchable());

        fd = JPASearchCoreFieldProcessor.processField("myEnum", Collections.emptyMap(), searchableFields, true, false, false);
        assertNotNull(fd);
        assertEquals("myEnum", fd.getPath());
        assertEquals("myEnum", fd.getEntityKey());
        assertEquals(JPASearchType.ENUM, fd.getSearchType());
        assertEquals(MyEnum.class, fd.getType());
        assertNotNull(fd.getSearchable());

        assertThrows(InvalidFieldException.class, () -> JPASearchCoreFieldProcessor.processField("test", Collections.emptyMap(), searchableFields, true, false, false));
        assertThrows(InvalidFieldException.class, () -> JPASearchCoreFieldProcessor.processField("offsetTime", Collections.emptyMap(), searchableFields, true, true, true));

        fd = JPASearchCoreFieldProcessor.processField("test", Collections.emptyMap(), searchableFields, false, false, false);
        assertNull(fd);
    }

    // ----- Item #18: entityFieldMap behaviour when the searchable field uses @Tag --------------------------------

    /**
     * Documents the precedence of entityFieldMap when the searchable field is declared with tags.
     * <p>
     * MyModel has:
     * <pre>
     *   @Searchable(tags = {
     *       @Tag(fieldKey = "wrapperLong.one"),
     *       @Tag(fieldKey = "wrapperLong.two", entityFieldKey = "wrapperLongYes")
     *   })
     *   private Long wrapperLong;
     * </pre>
     * <p>
     * The map key must match the <em>tag fieldKey</em> (e.g. "wrapperLong.two"), not the
     * underlying model field name ("wrapperLong"). The latter never produces a valid descriptor.
     */
    @Test
    public void processField_entityFieldMap_overridesTagEntityKey_whenKeyMatchesTagFieldKey() {
        var searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);

        var fd = JPASearchCoreFieldProcessor.processField(
                "wrapperLong.two",
                Map.of("wrapperLong.two", "alias.replaced"),
                searchableFields, true, false, false);

        assertNotNull(fd);
        assertEquals("wrapperLong.two", fd.getPath());
        assertEquals("alias.replaced", fd.getEntityKey(),
                "entityFieldMap[<tag fieldKey>] takes precedence over the tag's entityFieldKey");
    }

    @Test
    public void processField_entityFieldMap_keyedOnRawModelField_isIgnoredForTaggedField() {
        // The raw field name "wrapperLong" is NOT a usable searchable key (tag-only field), so the
        // entityFieldMap entry attached to it is irrelevant and the call still throws.
        var searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);

        assertThrows(InvalidFieldException.class, () -> JPASearchCoreFieldProcessor.processField(
                "wrapperLong",
                Map.of("wrapperLong", "ignored.alias"),
                searchableFields, true, false, false));
    }

    @Test
    public void processField_tagWithoutEntityFieldKey_fallsBackToTagFieldKey() {
        // wrapperLong.one has no explicit entityFieldKey on the @Tag → the entity key equals the tag fieldKey.
        var searchableFields = ReflectionUtils.getAllSearchableFields(MyModel.class);

        var fd = JPASearchCoreFieldProcessor.processField(
                "wrapperLong.one", Collections.emptyMap(), searchableFields, true, false, false);

        assertNotNull(fd);
        assertEquals("wrapperLong.one", fd.getEntityKey());
    }
}
