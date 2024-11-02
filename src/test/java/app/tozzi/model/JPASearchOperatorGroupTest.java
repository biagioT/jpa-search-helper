package app.tozzi.model;

import app.tozzi.exception.JPASearchException;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class JPASearchOperatorGroupTest {

    @Test
    public void load() {
        assertEquals(JPASearchOperatorGroup.OR, JPASearchOperatorGroup.load("or"));
        assertEquals(JPASearchOperatorGroup.AND, JPASearchOperatorGroup.load("and"));
        assertEquals(JPASearchOperatorGroup.NOT, JPASearchOperatorGroup.load("not"));
        assertThrows(JPASearchException.class, () -> JPASearchOperatorGroup.load("test"));
        assertThrows(JPASearchException.class, () -> JPASearchOperatorGroup.load("OR"));
    }

    @Test
    public void getAllValues() {
        var values = JPASearchOperatorGroup.getAllValues();
        assertNotNull(values);
        assertFalse(values.isEmpty());
        assertEquals(3, values.size());
    }
}
