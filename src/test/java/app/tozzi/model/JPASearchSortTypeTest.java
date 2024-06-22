package app.tozzi.model;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

public class JPASearchSortTypeTest {

    @Test
    public void loadSortTypeTest() {
        assertEquals(JPASearchSortType.ASC, JPASearchSortType.load("asc", null));
        assertEquals(JPASearchSortType.ASC, JPASearchSortType.load("ASC", null));
        assertEquals(JPASearchSortType.ASC, JPASearchSortType.load("asc", JPASearchSortType.ASC));
        assertEquals(JPASearchSortType.ASC, JPASearchSortType.load("ASC", JPASearchSortType.ASC));
        assertEquals(JPASearchSortType.DESC, JPASearchSortType.load("desc", null));
        assertEquals(JPASearchSortType.DESC, JPASearchSortType.load("DESC", null));
        assertEquals(JPASearchSortType.DESC, JPASearchSortType.load("desc", JPASearchSortType.DESC));
        assertEquals(JPASearchSortType.DESC, JPASearchSortType.load("DESC", JPASearchSortType.DESC));
        assertEquals(JPASearchSortType.DESC, JPASearchSortType.load("desc", JPASearchSortType.ASC));
        assertEquals(JPASearchSortType.ASC, JPASearchSortType.load("test", JPASearchSortType.ASC));
        assertNull(JPASearchSortType.load("test", null));
    }
}
