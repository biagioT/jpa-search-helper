package app.tozzi.model;

import app.tozzi.exception.JPASearchException;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class JPASearchPaginationFilterTest {

    @Test
    public void loadPaginationFilterTest() {
        assertEquals(JPASearchPaginationFilter.SORT, JPASearchPaginationFilter.load("sort"));
        assertEquals(JPASearchPaginationFilter.LIMIT, JPASearchPaginationFilter.load("limit"));
        assertEquals(JPASearchPaginationFilter.OFFSET, JPASearchPaginationFilter.load("offset"));
        assertThrows(JPASearchException.class, () -> JPASearchPaginationFilter.load("test"));
        assertThrows(JPASearchException.class, () -> JPASearchPaginationFilter.load("LIMIT"));
    }

    @Test
    public void loadKeysTest() {
        List<String> keys = JPASearchPaginationFilter.keys();
        assertNotNull(keys);
        assertFalse(keys.isEmpty());
        assertEquals(3, keys.size());
        assertTrue(keys.contains("limit"));
        assertTrue(keys.contains("sort"));
        assertTrue(keys.contains("offset"));
    }
}
