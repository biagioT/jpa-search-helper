package app.tozzi.test;

import app.tozzi.annotations.NestedSearchable;
import app.tozzi.annotations.Searchable;
import app.tozzi.annotations.Tag;
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

    @NestedSearchable
    private ExampleNestedBean nestedBean;

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

    @Searchable(tags = {
            @Tag(fieldKey = "f1"),
            @Tag(fieldKey = "f2"),
            @Tag(fieldKey = "t.f2"),
            @Tag(fieldKey = "t.f3", entityFieldKey = "ttt"),
            @Tag(fieldKey = "tf3", entityFieldKey = "tttee")
    })
    private String fieldName;

    @Searchable
    private boolean primitiveBoolean;

    @Searchable
    private Boolean wrapperBoolean;

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
