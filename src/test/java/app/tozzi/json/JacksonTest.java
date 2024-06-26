package app.tozzi.json;

import app.tozzi.model.input.JPASearchInput;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class JacksonTest {

    @Test
    public void serializeJpaSearchInputTest() throws JsonProcessingException {

        String json = """
                {
                                
                    "filter" : {
                        "operator" : "or",
                        "filters" : [
                            {
                                "operator" : "eq",
                                "value" : "1034567890123456",
                                "key" : "isbn"
                            },
                            {
                                "operator" : "in",
                                "values" : ["1034567890123456", "1234567890123456"],
                                "key" : "isbn"
                            },
                            {
                                "operator" : "null",
                                "key" : "isbn",
                                "options" : {
                                    "negate": true
                                }
                            },
                            {
                                "operator" : "and",
                                "filters" : [
                                    {
                                        "operator" : "null",
                                        "key" : "title",
                                        "options" : {
                                            "negate": true
                                        }
                                    },
                                    {
                                        "operator" : "gte",
                                        "key" : "pages",
                                        "value": 10
                                    }
                                ]
                            }
                        ]
                    },
                    "options" : {
                        "pageSize": 4
                    }
                                
                }
                """;

        ObjectMapper objectMapper = new ObjectMapper();
        var input = objectMapper.readValue(json, JPASearchInput.class);
        assertNotNull(input);
        assertNotNull(input.getFilter());
        assertEquals("or", input.getFilter().getOperator());
        assertNotNull(input.getFilter().getFilters());
        assertEquals(4, input.getFilter().getFilters().size());
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.FilterSingleValue fsv && fsv.getOperator().equals("eq") && fsv.getValue().equals("1034567890123456") && fsv.getKey().equals("isbn")));
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.FilterMultipleValues fsv && fsv.getOperator().equals("in") && fsv.getValues().contains("1034567890123456") && fsv.getValues().contains("1234567890123456") && fsv.getKey().equals("isbn")));
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.FilterSingleValue fsv && fsv.getOperator().equals("null") && fsv.getValue() == null && fsv.getOptions() != null && fsv.getOptions().isNegate() && fsv.getKey().equals("isbn")));
        assertTrue(input.getFilter().getFilters().stream().anyMatch(f -> f instanceof JPASearchInput.RootFilter rf && rf.getOperator().equals("and") && rf.getFilters() != null && rf.getFilters().size() == 2
                && rf.getFilters().getFirst() instanceof JPASearchInput.FilterSingleValue fsvSub1 && fsvSub1.getOperator().equals("null") && fsvSub1.getKey().equals("title") && fsvSub1.getValue() == null && fsvSub1.getOptions() != null && fsvSub1.getOptions().isNegate()
                && rf.getFilters().getLast() instanceof JPASearchInput.FilterSingleValue fsvSub2 && fsvSub2.getOperator().equals("gte") && fsvSub2.getKey().equals("pages") && fsvSub2.getValue() instanceof Integer i  && i == 10
        ));

    }

}
