package app.tozzi.model;

import org.junit.jupiter.api.Test;

import java.util.Collection;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

public class JPASearchTypeTest {

    @Test
    public void loadSearchType() {
        assertEquals(JPASearchType.LONG, JPASearchType.load(Long.class, null));
        assertEquals(JPASearchType.LONG, JPASearchType.load(long.class, null));
        assertEquals(JPASearchType.LONG, JPASearchType.load(Collection.class, JPASearchType.LONG));
        assertNull(JPASearchType.load(Collection.class, null));

    }
}
