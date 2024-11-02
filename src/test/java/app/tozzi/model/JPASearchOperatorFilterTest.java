package app.tozzi.model;

import app.tozzi.exception.JPASearchException;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class JPASearchOperatorFilterTest {

    @Test
    public void load() {
        assertEquals(JPASearchOperatorFilter.EQ, JPASearchOperatorFilter.load("eq"));
        assertEquals(JPASearchOperatorFilter.EMPTY, JPASearchOperatorFilter.load("empty"));
        assertThrows(JPASearchException.class, () -> JPASearchOperatorFilter.load("EQ"));
        assertThrows(JPASearchException.class, () -> JPASearchOperatorFilter.load("test"));
    }

    @Test
    public void getAllValues() {
        var values = JPASearchOperatorFilter.getAllValues();
        assertNotNull(values);
        assertFalse(values.isEmpty());
        assertEquals(12, values.size());
    }
}
