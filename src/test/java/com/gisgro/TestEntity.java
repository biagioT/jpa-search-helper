package com.gisgro;

import com.gisgro.annotations.NestedSearchable;
import com.gisgro.annotations.Searchable;
import com.gisgro.model.SearchType;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.math.BigDecimal;
import java.time.*;
import java.util.Date;

// Entity class
@Entity
@Getter
@Setter
@AllArgsConstructor
public class TestEntity {

    public TestEntity() {
    }

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

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

    @Searchable
    private long primitiveLong;

    @Searchable
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
    private String fieldName;

    @Searchable
    private boolean primitiveBoolean;

    @Searchable
    private Boolean wrapperBoolean;

    @NestedSearchable
    @ManyToOne
    @JoinColumn
    private TestEntity2 nested;

    @Searchable
    private TestEnum testEnum;

    @Searchable
    private Period period;
}
