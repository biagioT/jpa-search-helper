# JPA Search Helper

### Description
Library for building complex queries using JPA in Spring.

Do you want your controller to be able to receive a request like this and perform an advanced and complex search?
```
curl --request GET --url 'http://www.myexampledomain.com/persons?birthDate_gte=19910101&country=IT&firstName_eq=Biagio&lastName_iEndsWith=ZZI&company_in=COMP1,COMP2&notes_is=not_empty&birthDate_sort=ASC&_offset=0&_limit10'`
```

Read this readme!

### Prerequisites
- Java 17 or later

### Project dependency
#### Maven
```
<dependency>
    <groupId>app.tozzi</groupId>
    <artifactId>jpa-search-helper</artifactId>
    <version>0.0.2</version>
</dependency>
```

#### Gradle
```
implementation 'app.tozzi:jpa-search-helper:0.0.2'
```

### Managed search filters

| Filter name               | Library Key  | SQL                                    | Fixed value 
|---------------------------|--------------|----------------------------------------|---------------|
| Equals                    | _eq          | sql_col = val                          
| Equals (ignore case)      | _iEq         | UPPER(sql_col) = 'VAL'                 
| Contains                  | _contains    | sql_col LIKE '%val%'                   
| Contains (ignore case)    | _iContains   | UPPER(sql_col) LIKE '%VAL%'            
| In                        | _in          | sql_col IN (val1, val2, ..., valN)     
| Not In                    | _nin         | sql_col NOT IN (val1, val2, ..., valN) 
| Starts With               | _startsWith  | sql_col LIKE 'val%'                    
| Starts With (ignore case) | _iStartsWith | UPPER(sql_col) LIKE 'VAL%'             
| Ends With                 | _endsWith    | sql_col LIKE '%val'                    
| Ends With (ignore case)   | _iEndsWith   | UPPER(sql_col) LIKE '%VAL'             
| Not Equals                | _notEq       | sql_col <> val                         
| Not Equals (ignore case)  | _iNotEq      | UPPER(sql_col) <> 'VAL'                
| Greater Than              | _gt          | sql_col > val                          
| Greater Than or Equal     | _gte         | sql_col >= val                         
| Less Than                 | _lt          | sql_col < val                          
| Less Than or Equal        | _lte         | sql_col <= val                         
| Between                   | _between     | sql_col BETWEEN val1 AND val2          
| Null                      | _is | sql_col IS NULL                        | 'null'
| Empty                     | _is | sql_col IS NULL                        | 'empty'
| Not Null                  | _is | sql_col IS NOT NULL                    | 'not_null'
| Not Empty                 | _is | sql_col IS NOT NULL                    | 'not_empty'


### Pagination

| Filter name | Key | Fixed values
|--|--|--|
| Limit (page size) | _limit
| Offset (page number) | _offset
| Sort | _sort | ASC, DESC

### Usage

#### Searchable annotation
Annotate your DTO class with `@Searchable`, or alternatively you can apply the annotation directly on your entity class.
Example:

```java
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
```


The annotation allows you to specify:

- Core properties:

  - *entityFieldKey*:
    the name of the field defined on the entity bean (not to be specified if using the annotation on the entity bean). If not specified the key will be the field name.
  - *targetType*: the managed object type by entity. If not specified the librariy tries to obtain it based on field type (es. Integer field without target type definition will be INTEGER). If there is no type compatible with those managed, it will be managed as a string. Managed types: 
  
      - STRING, INTEGER, DOUBLE, FLOAT, LONG, BIGDECIMAL, BOOLEAN, DATE, LOCALDATE, LOCALDATETIME, LOCALTIME, OFFSETDATETIME, OFFSETTIME.

- Validation properties:
  
  - *datePattern*: only for DATE targetType. Defines the date pattern to use.
  - *maxSize, minSize*: maximum/minimum length of the value
  - *maxDigits, minDigits*: only for numeric types. Maximum/minimum number of digits.
  - *regexPattern*: regex pattern.
  - *decimalFormat*: only for decimal numeric types. Default #.##
  

Exceptions:
- If a field does not exist or is not searchable you will receive an `InvalidFieldException`.
- If the value of a field does not meet the requirements you will receive an `InvalidValueException`.

##### Example
DTO Bean:

```java
@Data
public class Person {

    @Searchable
    private String firstName;

    @Searchable
    private String lastName;

    @Searchable(entityFieldKey = "dateOfBirth")
    private Date birthDate;

    @Searchable
    private String country;
    
    @Searchable
    private String notes;

    @Searchable(entityFieldKey = "companyEntity.name")
    private String company;

}
```

Entity bean:

```java
@Entity
@Data
public class PersonEntity {

    @Id
    private Long id;

    @Column(name = "FIRST_NAME")
    private String firstName;

    @Column(name = "LAST_NAME")
    private String lastName;

    @Column(name = "BIRTH_DATE")
    private Date dateOfBirth;

    @Column(name = "COUNTRY")
    private String country;
    
    @Column(name = "NOTES")
    private String notes;
        
    @OneToOne
    private Company companyEntity;

}
```

#### JPASearchRepository
Your Spring JPA repository must extend JPASearchRepository<?>.
Example:

```java
@Repository
public interface PersonRepository extends JpaRepository<PersonEntity, Long>, JPASearchRepository<PersonEntity> {

}
```

In your service/manager bean define a map <filter_key, value>:
```java
// ...

Map<String, String> filters = new HashMap<>();  
filters.put("birthDate_gte", "19910101");  
filters.put("country", "IT");
filters.put("firstName_eq", "Biagio");
filters.put("lastName_iEndsWith", "ZZI");
filters.put("company_in", "Comp1,Comp2");
filters.put("notes_is", "not_empty");

// Without pagination
List<PersonEntity> fullSearch = personRepository.findAll(filters, Person.class);

filters.put("birthDate_sort" : "ASC");
filters.put("_limit", "10");
filters.put("_offset", "0");

// With pagination
Page<PersonEntity> orderedAndPaginatedSearch = personRepository.findAllWithPaginationAndSorting(filters, Person.class);

// ...
```

End.

### Spring Boot Project example with HTTP Endpoint
Controller:
```java
@RestController
public class MyController {
    
    @Autowired 		
    private PersonManager personManager;
         
    @GetMapping(path="/persons", produces = MediaType.APPLICATION_JSON_VALUE)  
    public List<Person> findPersons(@RequestParam Map<String, String> requestParams) {  
        return personManager.find(requestParams);  
    }
}
```

Manager:
```java
@Service 
public class PersonManager { 	
    	
    @Autowired 		
    private PersonRepository personRepository;
    		 		
    public List<Person> find(Map<String, String> filters) { 		
	    return personRepository.findAllWithPaginationAndSorting(filters, Person.class).stream().map(this::toDTO).toList(); 
	} 

    private static Person toDTO(PersonEntity personEntity) {
        // ...
    }

}
```
Curl:
```
curl --request GET --url 'http://www.myexampledomain.com/persons?birthDate_gte=19910101&country=IT&firstName_eq=Biagio&lastName_iEndsWith=ZZI&company_in=COMP1,COMP2&notes_is=not_empty&birthDate_sort=ASC&_offset=0&_limit10'`
```

### Join Fetch
It is possible to force joins with fetch to allow Hibernate to execute a single query for the relationships defined on the entity. **This is only possible without pagination**:
```java
// ...

Map<String, JoinFetch> fetches = Map.of("company", JoinFetch.LEFT);
personRepository.findAll(filters, Person.class, fetches);

// ...
```

### Multiple entities for the same DTO
If you have a DTO that is the result of the conversion of multiple entities, it is possible to explicitly specify a map (string, string) whose key represents the name of the DTO field and the value is the name of the field of the entity to be searched for:
```java
// ...

Map<String, String> entityFieldMap = Map.of("company", "companyEntity.name");

// Without pagination
personRepository.findAll(filters, Person.class, fetches, entityFieldMap);

// With pagination
personRepository.findAllWithPaginationAndSorting(filters, Person.class, entityFieldMap);

// ...
```