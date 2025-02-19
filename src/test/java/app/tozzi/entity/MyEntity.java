package app.tozzi.entity;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.OneToOne;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.UuidGenerator;

import java.math.BigDecimal;
import java.time.*;
import java.util.Date;
import java.util.UUID;

@Data
@Builder
@Entity
@NoArgsConstructor
@AllArgsConstructor
public class MyEntity {

    @Id
    private Long id;

    @UuidGenerator
    private UUID uuid;
    private String stringOne;
    private String stringTwo;
    private String stringThree;
    private String stringFalse;
    private String email;
    private int primitiveInteger;
    private Integer wrapperInteger;
    private Date stringDate;
    private Date dateOne;
    private long primitiveLong;
    private Long wrapperLongYes;
    private float primitiveFloat;
    private Float wrapperFloat;
    private double primitiveDoubleYes;
    private Double wrapperDouble;
    private BigDecimal bigDecimal;
    private LocalDateTime localDateTime;
    private LocalDate localDate;
    private LocalTime localTime;
    private OffsetDateTime offsetDateTime;
    private ZonedDateTime zonedDateTime;
    private OffsetTime offsetTime;
    private boolean primitiveBoolean;
    private Boolean wrapperBoolean;
    private String notSearchableOne;
    private String notSearchableTwo;
    private Long notSearchableThree;

    @OneToOne(cascade = CascadeType.ALL)
    private TestEntity1 test1;

    @OneToOne(cascade = CascadeType.ALL)
    private TestEntity2 test2;

    @OneToOne(cascade = CascadeType.ALL)
    private TestEntity3 test3;

}
