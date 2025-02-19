package app.tozzi.core;

import app.tozzi.exception.InvalidFieldException;
import app.tozzi.model.FieldDescriptor;
import app.tozzi.model.JPASearchType;
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

        assertThrows(InvalidFieldException.class, () -> JPASearchCoreFieldProcessor.processField("test", Collections.emptyMap(), searchableFields, true, false, false));
        assertThrows(InvalidFieldException.class, () -> JPASearchCoreFieldProcessor.processField("offsetTime", Collections.emptyMap(), searchableFields, true, true, true));

        fd = JPASearchCoreFieldProcessor.processField("test", Collections.emptyMap(), searchableFields, false, false, false);
        assertNull(fd);
    }

}
