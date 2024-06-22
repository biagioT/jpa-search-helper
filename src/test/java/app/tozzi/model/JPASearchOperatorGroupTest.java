package app.tozzi.model;

import app.tozzi.exception.JPASearchException;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class JPASearchOperatorGroupTest {

   @Test
   public void loadTest() {
       assertEquals(JPASearchOperatorGroup.OR, JPASearchOperatorGroup.load("or"));
       assertEquals(JPASearchOperatorGroup.AND, JPASearchOperatorGroup.load("and"));
       assertEquals(JPASearchOperatorGroup.NOT, JPASearchOperatorGroup.load("not"));
       assertThrows(JPASearchException.class, () -> JPASearchOperatorGroup.load("test"));
       assertThrows(JPASearchException.class, () -> JPASearchOperatorGroup.load("OR"));
   }
}
