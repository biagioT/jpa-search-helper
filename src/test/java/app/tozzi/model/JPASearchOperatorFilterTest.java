package app.tozzi.model;

import app.tozzi.exception.JPASearchException;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class JPASearchOperatorFilterTest {

   @Test
   public void loadTest() {
       assertEquals(JPASearchOperatorFilter.EQ, JPASearchOperatorFilter.load("eq"));
       assertEquals(JPASearchOperatorFilter.EMPTY, JPASearchOperatorFilter.load("empty"));
       assertThrows(JPASearchException.class, () -> JPASearchOperatorFilter.load("EQ"));
       assertThrows(JPASearchException.class, () -> JPASearchOperatorFilter.load("test"));
   }
}
