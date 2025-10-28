package app.tozzi.model;

import app.tozzi.annotation.*;
import lombok.Data;

import java.math.BigDecimal;
import java.time.*;
import java.util.Date;
import java.util.List;
import java.util.UUID;

@Data
public class MyModel {

    @Projectable
    @Searchable(targetType = JPASearchType.LONG)
    private String id;

    @Searchable
    @Projectable
    private UUID uuid;

    @Searchable(targetType = JPASearchType.ENUM)
    @Projectable
    private MyEnum myEnum;

    @Searchable(targetType = JPASearchType.ENUM, ordinalEnum = true)
    @Projectable
    private MyEnum myEnum2;

    @Searchable
    private String stringOne;

    @Searchable(sortable = false)
    private String stringTwo;

    @Searchable(sortable = false, trim = true)
    private String stringThree;

    @Searchable(allowLikeFilters = false)
    private String stringFalse;

    @Projectable(entityFieldKey = "email")
    @Searchable(regexPattern = "^[a-zA-Z0-9_!#$%&’*+/=?`{|}~^.-]+@[a-zA-Z0-9.-]+$", entityFieldKey = "email")
    private String stringMail;

    @Searchable(minSize = 5, maxSize = 10)
    private int primitiveInteger;

    @Searchable(minDigits = 2, maxDigits = 4)
    private Integer wrapperInteger;

    @Searchable(targetType = JPASearchType.DATE, datePattern = "yyyyMMdd")
    private String stringDate;

    @Searchable(datePattern = "yyyy-MM-dd HH:mm:ss", allowedFilters = {JPASearchOperatorFilter.BETWEEN, JPASearchOperatorFilter.GT})
    private Date dateOne;

    @Searchable
    private long primitiveLong;

    @Searchable(tags = {
            @Tag(fieldKey = "wrapperLong.one"),
            @Tag(fieldKey = "wrapperLong.two", entityFieldKey = "wrapperLongYes")
    })
    private Long wrapperLong;

    @Searchable(decimalFormat = "#.###")
    private float primitiveFloat;

    @Searchable
    private Float wrapperFloat;

    @Searchable(entityFieldKey = "primitiveDoubleYes")
    private double primitiveDouble;

    @Searchable(decimalFormat = "#.000")
    private Double wrapperDouble;

    @Searchable
    private BigDecimal bigDecimal;

    @Searchable(datePattern = "yyyy-MM-dd'T'HH:mm:ss", notAllowedFilters = JPASearchOperatorFilter.LT)
    private LocalDateTime localDateTime;

    @Searchable(datePattern = "yyyy-MM-dd")
    private LocalDate localDate;

    @Searchable(datePattern = "HHmmssXXX")
    private LocalTime localTime;

    @Searchable(datePattern = "yyyy-MM-dd'T'HH:mm:ssXXX")
    private OffsetDateTime offsetDateTime;

    @Searchable(datePattern = "yyyy-MM-dd'T'HH:mm:ss.SSSXXXXX")
    private ZonedDateTime zonedDateTime;

    @Searchable(datePattern = "HH:mm:ss.SSSXXXXX", sortable = false)
    private OffsetTime offsetTime;

    @Searchable
    private boolean primitiveBoolean;

    @Searchable
    private Boolean wrapperBoolean;

    @NestedProjectable
    @NestedSearchable
    private MySubModel mySubModel;

    private String notSearchableOne;
    private String notSearchableTwo;
    private Long notSearchableThree;

    @NestedProjectable
    @NestedSearchable
    private List<MyOtherSubModel> list;

    @Searchable(elementCollection = true)
    private List<String> keywords;

    @Data
    public static class MySubModel {

        @Projectable(entityFieldKey = "test2.colTest2")
        @Searchable(entityFieldKey = "test2.colTest2")
        private String searchMe;

        @NestedSearchable
        private MySubSubModel mySubSubModel;

        private BigDecimal notSearchable;

    }

    @Data
    public static class MySubSubModel {

        @Searchable(entityFieldKey = "test1.colTest1")
        private String searchMeAgain;

        private BigDecimal notSearchableNo;

    }

    @Data
    public static class MyOtherSubModel {

        @Projectable(entityFieldKey = "test1.entity6s.colTest6")
        @Searchable(entityFieldKey = "test1.entity6s.colTest6")
        private String other;
    }
}
