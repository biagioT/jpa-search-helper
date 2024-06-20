package app.tozzi;

import app.tozzi.annotations.NestedSearchable;
import app.tozzi.annotations.Searchable;
import app.tozzi.annotations.Tag;
import app.tozzi.model.SearchType;
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

    public TestEntity() {}

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

    @NestedSearchable
    @ManyToOne
    @JoinColumn
    private TestEntity2 nestedBean;

    @Searchable
    private TestEnum testEnum;

    @Searchable
    private Period period;
}
