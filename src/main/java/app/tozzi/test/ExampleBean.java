package app.tozzi.test;

import app.tozzi.annotations.Searchable;
import app.tozzi.model.SearchType;
import lombok.Data;

import java.math.BigDecimal;
import java.time.*;
import java.util.Date;

@Data
public final class ExampleBean {

    @Searchable(minSize = 5, maxSize = 10)
    private int primitiveInteger;

    @Searchable(minDigits = 2, maxDigits = 4)
    private Integer wrapperInteger;

    private String string;

    @Searchable(regexPattern = "^[a-zA-Z0-9_!#$%&’*+/=?`{|}~^.-]+@[a-zA-Z0-9.-]+$")
    private String email;

    @Searchable(targetType = SearchType.INTEGER)
    private String integerString;

    @Searchable(targetType = SearchType.DATE, datePattern = "yyyyMMdd")
    private String dateString;

    @Searchable(datePattern = "yyyyMMdd")
    private Date date1;

    @Searchable(targetType = SearchType.DATE, datePattern = "yyyyMMdd")
    private Date date2;

    @Searchable(entityFieldKey = "entity.long-one")
    private long primitiveLong;

    @Searchable(entityFieldKey = "entity.long-two")
    private Long wrapperLong;

    @Searchable(decimalFormat = "#.#")
    private float primitiveFloat;

    @Searchable
    private Float wrapperFloat;

    @Searchable(decimalFormat = "#.#")
    private double primitiveDouble;

    @Searchable(decimalFormat = "#.#")
    private Double wrapperDouble;

    @Searchable
    private BigDecimal bigDecimal;

    @Searchable(datePattern = "yyyy-MM-dd'T'HH:mm:ss")
    private LocalDateTime localDateTime;

    @Searchable(datePattern = "yyyy-MM-dd")
    private LocalDate localDate;

    @Searchable(datePattern = "HHmmssXXX")
    private LocalTime localTime;

    @Searchable(datePattern = "yyyy-MM-dd'T'HH:mm:ssXXX")
    private OffsetDateTime offsetDateTime;

    @Searchable(datePattern = "HHmmssXXX")
    private OffsetTime offsetTime;

    @Searchable
    private boolean primitiveBoolean;

    @Searchable
    private Boolean wrapperBoolean;

    private ExampleNestedBean nestedBean;

    @Data
    public static class ExampleNestedBean {

        @Searchable
        private String string;

        @Searchable
        private String string2;

        @Searchable
        private String string3;

        @Searchable
        private String string4;

        @Searchable
        private String string5;

        @Searchable
        private String string6;

        @Searchable
        private String string7;

    }
}
