
# JPA Search Helper

Library for building and running advanced and dynamic queries using JPA in Spring Boot.

## TL;DR
### Key features
- **Queries**: the library supports two modes for building advanced and dynamic queries:
  -  *Mode 1*: Via `Map<String, String>`, to support GET endpoints with query params.
  -  *Mode 2*: Via an [object](https://github.com/biagioT/jpa-search-helper/blob/main/src/main/java/app/tozzi/model/input/JPASearchInput.java), to support POST endpoints that expect query parameters in the body (from *2.0.0* version)
- **Projection**:  for both modes, the library allows you to extract only a subselection of fields from the query (from *3.2.0* version)

### Spoiler
Through **jpa-search-helper** your controller* will be able to receive requests like this:

*Mode 1*:
```bash  
curl -X GET \
  'https://myexampledomain.com/persons?
  firstName=Biagio
  &lastName_startsWith=Toz
  &birthDate_gte=19910101
  &country_in=IT,FR,DE
  &address_eq=Via Roma 1,Via Milano/,1,20 West/,34th Street
  &company.name_in=Microsoft,Apple,Google
  &company.employees_between=500,5000'
```  

*Mode 2*:
```bash  
curl -X POST -H "Content-type: application/json" -d '{
  "filter" : {
      "operator": "and", // the first filter must contain a root operator: AND, OR or NOT
      "filters" : [
        {
          "operator": "eq",
          "key": "firstName",
          "value": "Biagio"
        },
        {
          "operator": "or",
          "filters": [
            {
              "operator": "startsWith",
              "key": "lastName",
              "value": "Toz",
              "options": {
                "ignoreCase": true
              }
            },
            {
              "operator": "endsWith",
              "key": "lastName",
              "value": "ZZI",
              "options": {
                "ignoreCase": true,
                "trim" : true
              }
            }
          ]
        },
        {
          "operator": "in",
          "key": "company.name",
          "values": ["Microsoft", "Apple", "Google"]
        },
        {
          "operator": "or",
          "filters": [
            {
              "operator": "gte",
              "key": "birthDate",
              "value": "19910101"
            },
            {
              "operator": "lte",
              "key": "birthDate",
              "value": "20010101"
            }
          ]
        },
        {
          "operator": "between",
          "key" : "company.employees",
          "values": [500, 5000],
          "options": {
            "negate": true
          }
        }
      ]
  },
  "options": {
    "pageSize": 10,
    "pageOffset": 0,
    "sortOptions" : [
      {
          "key": "birthDate",
          "desc": false
      }
    ]
  }
  
}' 'https://myexampledomain.com/persons'
```  

..how you do it? Read this readme!

**\*** Please note:  the library does not expose controllers/HTTP Endpoints, but only offers the repository that will build and execute the queries.

## Compatibility Matrix

#### Minimum requirements
- Java 17 or later
- Spring Boot 3.2.x or later

| JPA Search Helper | Spring Boot | Java      |  
|-------------------|-------------|-----------|  
| [v0.0.1 - v2.1.1] | 3.2.x       | [17 - 25] |  
| [v3.0.0 - v3.2.2] | 3.3.x       | [17 - 25] |  
| [v3.3.0 - latest] | 3.4.x       | [17 - 25] |  
| [v3.5.0 - latest] | 3.5.x       | [17 - 25] |  

## Project dependency
#### Maven
```  
<dependency>  
 <groupId>app.tozzi</groupId> 
 <artifactId>jpa-search-helper</artifactId> 
 <version>3.5.4</version>
</dependency>  
```  

#### Gradle
```  
implementation 'app.tozzi:jpa-search-helper:3.5.4'
```

## Queries - Usage
### 1. `@Searchable` annotation
Start by applying the `@Searchable` annotation to the fields in your Domain Model, or alternatively your JPA entity, **that you want to make available for search**.
If you have fields that you want to make searchable within other objects then annotate these with `@NestedSearchable`.

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
  private String fillerOne;
  
  @Searchable
  private String fillerTwo;

  @NestedSearchable
  private Company company;

  @Data
  public static class Company {

    @Searchable(entityFieldKey= "companyEntity.name")
    private String name;

    @Searchable(entityFieldKey= "companyEntity.employeesCount")
    private int employees;
  }
}
```
The annotation allows you to specify:

- Core properties:

  - `entityFieldKey`:
    the name of the field defined on the entity bean (not to be specified if using the annotation on the entity bean). If not specified the key will be the field name.
  - `targetType`: the managed object type by entity. If not specified the librariy tries to obtain it based on field type (es. Integer field without target type definition will be `INTEGER`). If there is no type compatible with those managed, it will be managed as a string. Managed types:

    - `STRING`, `INTEGER`, `DOUBLE`, `FLOAT`, `LONG`, `BIGDECIMAL`, `BOOLEAN`, `DATE`, `LOCALDATE`, `LOCALDATETIME`, `LOCALTIME`, `OFFSETDATETIME`, `OFFSETTIME`, `ZONEDDATETIME`, `ENUM`, `DATE_SQL`, `TIME_SQL`, `INSTANT`, `TIMESTAMP`.

- Validation properties:

  - `datePattern`: only for `DATE`, `LOCALDATE`, `LOCALDATETIME`, `LOCALTIME`, `OFFSETDATETIME`, `OFFSETTIME`, `ZONEDDATETIME`, `TIMESTAMP`, `TIME_SQL`, `DATE_SQL`, `INSTANT` target types. Defines the date pattern to use.
  - `maxSize, minSize`: maximum/minimum length of the value.
  - `maxDigits, minDigits`: only for numeric types. Maximum/minimum number of digits.
  - `regexPattern`: regex pattern.
  - `decimalFormat`: only for decimal numeric types. Default `#.##`

- Other:
  - `sortable`: if false, the field can be used by search but cannot be used for sorting. Default: true.
  - `trim`: apply trim.
  - `tags`: useful if the Domain Model field can correspond to multiple entity fields (the example is available further down).
  - `allowedFilters`: exclusively allowed filters.
  - `notAllowedFilters`: not allowed filters.
  - `likeFilters`: allowed like filters (_contains_, _startsWith_, _endsWith_). Default: true.
  - `ordinalEnum`: only for `ENUM` type; true if search via ordinal

Continuing the example, our entity classes:

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

  @Column(name = "FIL_ONE")
  private String fillerOne;

  @Column(name = "FIL_TWO")
  private String fillerTwo;

  @OneToOne
  private CompanyEntity companyEntity;

}

@Entity
@Data
public class CompanyEntity {

  @Id
  private Long id;

  @Column(name = "NAME")
  private String name;

  @Column(name = "COUNT")
  private Integer employeesCount;

}
```

### 2. `JPASearchRepository` interface
Your Spring JPA repository must extend `JPASearchRepository<YourEntityClass>`.

```java
@Repository
public interface PersonRepository extends JpaRepository<PersonEntity, Long>, JPASearchRepository<PersonEntity> {

}
```

### 3. Search implementation
In your manager, or in your service, or wherever you want to use the repository:

**Mode 1**: define a map _<filter_key#options, value>_:
```java
// ...

  @Autowired
  private PersonRepository personRepository;

  public List<Person> advancedSearch() {
  
	// Pure example, in real use case it is expected that these filters can be passed directly by the controller
    Map<String, String> filters = new HashMap<>();
    filters.put("firstName_eq", "Biagio");
    filters.put("lastName_startsWith#i", "Toz"); // ignore case
    filters.put("birthDate_gte", "19910101"); 
    filters.put("country_in", "IT,FR,DE");
    filters.put("company.name_eq#n", "Bad Company"); // negation
    filters.put("company.employees_between", "500,5000");
    filters.put("fillerOne_null#n", "true"); // not null
    filters.put("fillerTwo_empty", "true"); // empty
    
    // Without pagination
    List<PersonEntity> fullSearch = personRepository.findAll(filters, Person.class);
  
    filters.put("birthDate_sort" : "ASC"); // sorting key and sorting order
    filters.put("_limit", "10"); // page size
    filters.put("_offset", "0"); // page offset
    
    // With pagination
    Page<PersonEntity> sortedAndPaginatedSearch = personRepository.findAllWithPaginationAndSorting(filters, Person.class);
    
    // ...
  }

// ...
```

**Mode 2**: instead of a map, you will need to use [`JPASearchInput`](https://github.com/biagioT/jpa-search-helper/blob/main/src/main/java/app/tozzi/model/input/JPASearchInput.java), shown here, for simplicity, in JSON format.

```json
{
  "filter" : {
      "operator": "and", // the first filter must contain a root operator: AND, OR or NOT
      "filters" : [
        {
          "operator": "eq",
          "key": "firstName",
          "value": "Biagio"
        },
        {
          "operator": "or",
          "filters": [
            {
              "operator": "startsWith",
              "key": "lastName",
              "value": "Toz",
              "options": {
                "ignoreCase": true
              }
            },
            {
              "operator": "endsWith",
              "key": "lastName",
              "value": "ZZI",
              "options": {
                "ignoreCase": true,
                "trim" : true
              }
            }
          ]
        },
        {
          "operator": "in",
          "key": "company.name",
          "values": ["Microsoft", "Apple", "Google"]
        },
        {
          "operator": "or",
          "filters": [
            {
              "operator": "gte",
              "key": "birthDate",
              "value": "19910101"
            },
            {
              "operator": "lte",
              "key": "birthDate",
              "value": "20010101"
            }
          ]
        },
        {
          "operator": "empty",
          "key": "fillerOne",
          "options": {
            "negate": true
          }
        },
        {
          "operator": "between",
          "key" : "company.employees",
          "values": [500, 5000],
          "options": {
            "negate": true
          }
        }
      ]
  },
  "options": {
    "pageSize": 10,
    "pageOffset": 0,
    "sortOptions" : [
      {
        "key": "birthDate",
        "desc": false
      }
    ]
  }
  
}
```
Through *Mode 2* it is possible to manage complex filters with `AND`, `OR` and `NOT` (see later).

#### Exceptions:
- If a field does not exist, is not searchable or is not sortable, you will receive an `InvalidFieldException`.
- If the value of a field does not meet the requirements you will receive an `InvalidValueException`.
- Other cases: `JPASearchException`

## Queries - Docs
### Managed root operators
| Filter name | Library Key | Supported modes |
|-------------|-------------|-----------------|
| AND         | and         | 1, 2            |
| OR          | or          | 2               |
| NOT         | not         | 2               |

Through *Mode 1*, all filters compose exclusively an `AND` search.

To use the other operators, `OR` and `NOT`, you must use *Mode 2*

### Managed search filter operators
| Filter name               | Library Key | SQL                                | Supported modes | Value required |
|---------------------------|-------------|------------------------------------|-----------------|----------------|
| Equals                    | eq          | sql_col = val                      | 1,2 | yes            |
| Contains                  | contains    | sql_col LIKE '%val%'               | 1,2 | yes            |
| In                        | in          | sql_col IN (val1, val2, ..., valN) | 1,2 | yes            |
| Starts With               | startsWith  | sql_col LIKE 'val%'                | 1,2 | yes            |
| Ends With                 | endsWith    | sql_col LIKE '%val'                | 1,2 | yes            |
| Greater Than              | gt          | sql_col > val                      | 1,2 | yes            |
| Greater Than or Equal     | gte         | sql_col >= val                     | 1,2 | yes            |
| Less Than                 | lt          | sql_col < val                      | 1,2 | yes            |
| Less Than or Equal        | lte         | sql_col <= val                     | 1,2 | yes            |
| Between                   | between     | sql_col BETWEEN val1 AND val2      | 1,2 | yes            |
| Null                      | null        | sql_col IS NULL                    | 1,2 | no             |
| Empty                     | empty       | sql_collection_col IS NULL               | 1,2 | no             |

#### Options
**Mode 1**
| Option description | Library Key |
|--------------------|-------------|
| Ignore case        | #i          |
| Negation           | #n          |
| Trim               | #t          |

The option keys must be appended to the filter; e.g. _?firstName_eq#i=Biagio_ or _?firstName_eq#i#n=Biagio_

**Mode 2**
| Option description | Library Key (Java attributes) |
|------------------|-------------------------------|
| Ignore case      | ignoreCase                    |
| Negation      | negate                        |
| Trim | trim                          |

For each filter it is possible to define `options`
```json
{
  // ...
      {
         "operator": "eq",
         "key": "firstName",
         "value": "Biagio",
         "options": {
           "ignoreCase": true,
           "trim": false,
           "negate": true
         }
      }
  // ...
}
```
Java object:
```java  
@Data  
public static class JPASearchFilterOptions {  
 private boolean ignoreCase;  
 private boolean trim;  
 private boolean negate;  
}  
```

### Pagination

| Filter name | Key | Fixed values
|--|--|--|
| Limit (page size) | limit
| Offset (page number) | offset
| Sort | sort | ASC, DESC

**Mode 1**: e.g. *?firstName_sort=DESC&_limit=10&_offset=0*

**Mode 2**: value root `options`:
```json
{
  "filter" : {
    // ...
  },
  "options" : {
    "sortOptions" : [
      {
        "key": "firstName",
        "desc": true
      }
    ],
    "pageSize": 10,
    "pageOffset": 1
  }
}
```
Java object:
```java
@Data
public static class JPASearchOptions {
    private List<JPASortOptions> sortOptions;
    private Integer pageSize;
    private Integer pageOffset;
    private List<String> selections;
}

@Data
public static class JPASortOptions {

  @NotEmpty
  @NotNull
  private String key;

  private Boolean desc = false;
}
```

### Other (only for Mode 1)
- Separator for array values: `,`: e.g. _?myField_in=test1,test2_ --> values to search for: ["**test1**", "**test2**"]
- To escape separator: `/,`: e.g. _?myField_in=test1,test2/,test3_ --> values to search for: ["**test1**", "**test2,test3**"]

## Projection - Usage

### 1. `@Projectable` annotation
Start by applying the `@Projectable` annotation to the fields in your Domain Model, or alternatively your JPA entity, **that you want to make available for selection**.
If you have fields that you want to make selectable within other objects then annotate these with `@NestedProjectable`.

```java
@Data
public class Person {

  @Searchable
  private String firstName;

  @Projectable
  @Searchable
  private String lastName;

  @Projectable(entityFieldKey = "dateOfBirth")
  @Searchable(entityFieldKey = "dateOfBirth")
  private Date birthDate;

  @Searchable
  private String country;
  
  @Searchable
  private String fillerOne;
  
  @Searchable
  private String fillerTwo;

  @NestedProjectable
  @NestedSearchable
  private Company company;

  @Data
  public static class Company {

    @Searchable(entityFieldKey= "companyEntity.name")
    private String name;
	
	@Projectable(entityFieldKey= "companyEntity.employeesCount")
    @Searchable(entityFieldKey= "companyEntity.employeesCount")
    private int employees;
  }
}
```
The annotation allows you to specify:

- Core properties:

  - `entityFieldKey`:
    the name of the field defined on the entity bean (not to be specified if using the annotation on the entity bean). If not specified the key will be the field name.

### 2. `JPASearchRepository` interface
Your Spring JPA repository must extend `JPAProjectionRepository<YourEntityClass>`.

```java  
@Repository  
public interface PersonRepository extends JpaRepository<PersonEntity, Long>, JPASearchRepository<PersonEntity>, JPAProjectionRepository<PersonEntity> {  
  
}  
```

### 3. Projection implementation
In your manager, or in your service, or wherever you want to use the repository:

**Mode 1**: define (or add to the map used for the Mode 1 search) a map:

- key: *selections*
- value: the only fields that you want to extract separated by commas `,`

```java
// ...

  @Autowired
  private PersonRepository personRepository;

  public List<Person> advancedSearch() {
  
    // Pure example, in real use case it is expected that these filters can be passed directly by the controller
    Map<String, String> filters = new HashMap<>();
    filters.put("firstName_eq", "Biagio");
    filters.put("lastName_startsWith#i", "Toz"); // ignore case
    filters.put("birthDate_gte", "19910101"); 
    filters.put("country_in", "IT,FR,DE");
    filters.put("company.name_eq#n", "Bad Company"); // negation
    filters.put("company.employees_between", "500,5000");
    filters.put("fillerOne_null#n", "true"); // not null
    filters.put("fillerTwo_empty", "true"); // empty

    // Selections
    filters.put("selections", "lastName,birthDate,company.employees");
    
    // Without sorting
    List<Map<String, Object>> result = personRepository.projection(filters, Person.class, PersonEntity.class);
  
    filters.put("birthDate_sort" : "ASC"); // sorting key and sorting order
    
    // With sorting
    List<Map<String, Object>> sortedAndPaginatedSearch = personRepository.projectionWithSorting(filters, Person.class, PersonEntity.class);
    
    // ... convert the list of maps into your model
  }

// ...
```

**Mode 2**: instead of a map, you will need to use [`JPASearchInput`](https://github.com/biagioT/jpa-search-helper/blob/main/src/main/java/app/tozzi/model/input/JPASearchInput.java), shown here, for simplicity, in JSON format.
```json
{
  "filter" : {
      "operator": "and", // the first filter must contain a root operator: AND, OR or NOT
      "filters" : [
        {
          "operator": "eq",
          "key": "firstName",
          "value": "Biagio"
        },
        {
          "operator": "or",
          "filters": [
            {
              "operator": "startsWith",
              "key": "lastName",
              "value": "Toz",
              "options": {
                "ignoreCase": true
              }
            },
            {
              "operator": "endsWith",
              "key": "lastName",
              "value": "ZZI",
              "options": {
                "ignoreCase": true,
                "trim" : true
              }
            }
          ]
        },
        {
          "operator": "in",
          "key": "company.name",
          "values": ["Microsoft", "Apple", "Google"]
        },
        {
          "operator": "or",
          "filters": [
            {
              "operator": "gte",
              "key": "birthDate",
              "value": "19910101"
            },
            {
              "operator": "lte",
              "key": "birthDate",
              "value": "20010101"
            }
          ]
        },
        {
          "operator": "empty",
          "key": "fillerOne",
          "options": {
            "negate": true
          }
        },
        {
          "operator": "between",
          "key" : "company.employees",
          "values": [500, 5000],
          "options": {
            "negate": true
          }
        }
      ]
  },
  "options": {
    "pageSize": 10,
    "pageOffset": 0,
    "sortOptions" : [
      {
        "key": "birthDate",
        "desc": false
      }
    ],
    "selections" : [
		"lastName",
		"birthDate",
		"company.employees"
	]
  }
}
```

For both modes, the projection will return a List<Map<String, Object>> result where **the map structure and keys will reflect the entity structure** (to be clear *toJson(entityList) == toJson(mapList)*)

Note 1:
> Be careful: the default projection forces all Join relationships as LEFT JOIN. If you don't want this behavior, choose to use the repository methods (methods with 'Classic' suffix) that allow you to possibly modify only the relations you want to modify

Note 2:
> Projection, regardless of whether you want it or not, will always extract the fields that represent the primary keys of an entity (or related entities)

Note 3:

> Pagination is not supported

#### Exceptions:
- If a field does not exist, is not searchable, is not sortable or is not projectable, you will receive an `InvalidFieldException`.
- Other cases: `JPASearchException`

---

## Advanced Settings
### Join Fetch
It is possible to force joins with fetch to allow Hibernate (or your JPA framework) to execute a single query for the relationships defined on the entity. **This is only possible without pagination**:
```java
// ...

Map<String, JoinFetch> fetches = Map.of("companyEntity", JoinFetch.LEFT);
personRepository.findAll(filters, Person.class, fetches);

// ...
```

### Multiple entities for the same Domain Model
If you have a Domain Model that is the result of the conversion of multiple entities, it is possible to explicitly specify a map (string, string) whose key represents the name of the Domain Model field and the value is the name of the field of the entity to be searched for:
```java
// ...

Map<String, String> entityFieldMap = Map.of("company", "companyEntity.name");

// Without pagination
personRepository.findAll(filters, Person.class, fetches, entityFieldMap);

// With pagination
personRepository.findAllWithPaginationAndSorting(filters, Person.class, entityFieldMap);

// ...
```

### Multiple object for the same entity
Another special case could be where an object can be repeated within the Domain Model to represent multiple pieces of the entity. The solution for the search:
```java

@Entity
public class CoupleEntity {

  @Id
  private Long id;

  @Column(name = "p1_fn")
  private String p1FirstName;

  @Column(name = "p1_ln")
  private String p1LastName;

  @Column(name = "p2_fn")
  private String p2FirstName;

  @Column(name = "p2_ln")
  private String p2LastName;
}

@Data
public class Couple {

  @NestedSearchable
  private Person p1;

  @NestedSearchable
  private Person p2;

  @Data
  public static class Person {

    @Searchable(tags = {
            @Tag(fieldKey = "p1.firstName", entityFieldKey = "p1FirstName"),
            @Tag(fieldKey = "p2.firstName", entityFieldKey = "p2FirstName"),
    })
    private String firstName;

    @Searchable(tags = {
            @Tag(fieldKey = "p1.lastName", entityFieldKey = "p1LastName"),
            @Tag(fieldKey = "p2.lastName", entityFieldKey = "p2LastName"),
    })
    private String lastName;
  }
}
```

```bash
curl - request GET 
 - url 'https://www.myexampledomain.com/couples?
p1.firstName_iEq=Romeo
&p2.firstName_iEq=Giulietta'
```

---  
## Spring Boot Project example with HTTP Endpoint
Please note: this library does not expose any endpoints and therefore no controllers.
An example project, exhaustive and complete, is available  [here](https://github.com/biagioT/jpa-search-helper-demo).

#### Mode 1

Controller:
```java
@RestController
@RequestMapping("/persons")
public class PersonController {

  @Autowired
  private PersonManager personManager;

  @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
  public List<Person> findPersons(@RequestParam Map<String, String> requestParams) {
    return personManager.find(requestParams);
  }
  
  @GetMapping(path="/projection", produces = MediaType.APPLICATION_JSON_VALUE)
  public List<Person> projection(@RequestParam Map<String, String> requestParams) {
    return personManager.projection(requestParams);
  }
}
```

Service/Manager bean:
```java
@Service
public class PersonManager {

  @Autowired
  private PersonRepository personRepository;

  public List<Person> find(Map<String, String> filters) {
    return personRepository.findAllWithPaginationAndSorting(filters, Person.class).stream().map(this::toModel).toList();
  }
  
  public List<Person> projection(Map<String, String> filters) {
    return personRepository.projection(filters, Person.class, PersonEntity.class).stream().map(this::toModel).toList();
  }

  private static Person toModel(PersonEntity personEntity) {
    // ...
  }
  
  private static Person toModel(Map<String, Object> entityMap) {
    // ...
  }

}
```
Curl:
```bash
curl - X GET 
'http://localhost:8080/persons?
firstName=Biagio
&lastName_startsWith=Toz
&birthDate_gte=19910101
&country_in=IT,FR,DE
&company.name_in=Microsoft,Apple
&company.employees_between=500,5000'
```

or

```bash
curl - X GET 
'http://localhost:8080/persons/projection?
firstName=Biagio
&lastName_startsWith=Toz
&birthDate_gte=19910101
&country_in=IT,FR,DE
&company.name_in=Microsoft,Apple
&company.employees_between=500,5000
&selections=firstName,birthDate'
```

#### Mode 2

Controller:
```java
@RestController
@RequestMapping("/persons")
@Validated
public class PersonController {

  @Autowired
  private PersonManager personManager;

  @PostMapping(produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public List<Person> findPersons(@Valid @RequestBody JPASearchInput input) {
    return personManager.find(input);
  }
}

@PostMapping(path="/projection", produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public List<Person> projection(@Valid @RequestBody JPASearchInput input) {
    return personManager.projection(input);
  }
}
```

Service/Manager bean:
```java
@Service
public class PersonManager {

  @Autowired
  private PersonRepository personRepository;

  public List<Person> find(JPASearchInput input) {
    return personRepository.findAllWithPaginationAndSorting(input, Person.class).stream().map(this::toModel).toList();
  }
  
  public List<Person> find(JPASearchInput input) {
    return personRepository.projection(input, Person.class, PersonEntity.class).stream().map(this::toModel).toList();
  }

  private static Person toModel(PersonEntity entity) {
    // ...
  }
  
  private static Person toModel(Map<String, Object> entityMap) {
    // ...
  }

}
```
Curl:
```bash
curl -X POST -H "Content-type: application/json" -d '{
  "filter" : {
      "operator": "and", // the first filter must contain a root operator: AND, OR or NOT
      "filters" : [
        {
          "operator": "eq",
          "key": "firstName",
          "value": "Biagio"
        },
        {
          "operator": "or",
          "filters": [
            {
              "operator": "startsWith",
              "key": "lastName",
              "value": "Toz",
              "options": {
                "ignoreCase": true
              }
            },
            {
              "operator": "endsWith",
              "key": "lastName",
              "value": "ZZI",
              "options": {
                "ignoreCase": true,
                "trim" : true
              }
            }
          ]
        },
        {
          "operator": "in",
          "key": "company.name",
          "values": ["Microsoft", "Apple", "Google"]
        },
        {
          "operator": "or",
          "filters": [
            {
              "operator": "gte",
              "key": "birthDate",
              "value": "19910101"
            },
            {
              "operator": "lte",
              "key": "birthDate",
              "value": "20010101"
            }
          ]
        },
        {
          "operator": "between",
          "key" : "company.employees",
          "values": [500, 5000],
          "options": {
            "negate": true
          }
        }
      ]
  },
  "options": {
    "pageSize": 10,
    "pageOffset": 0,
    "sortOptions" : [
      {
        "key": "birthDate",
        "desc": false
      }
    ]
  }
  
}' 'http://localhost:8080/persons'
```
or
```bash
curl -X POST -H "Content-type: application/json" -d '{
  "filter" : {
      "operator": "and", // the first filter must contain a root operator: AND, OR or NOT
      "filters" : [
        {
          "operator": "eq",
          "key": "firstName",
          "value": "Biagio"
        },
        {
          "operator": "or",
          "filters": [
            {
              "operator": "startsWith",
              "key": "lastName",
              "value": "Toz",
              "options": {
                "ignoreCase": true
              }
            },
            {
              "operator": "endsWith",
              "key": "lastName",
              "value": "ZZI",
              "options": {
                "ignoreCase": true,
                "trim" : true
              }
            }
          ]
        },
        {
          "operator": "in",
          "key": "company.name",
          "values": ["Microsoft", "Apple", "Google"]
        },
        {
          "operator": "or",
          "filters": [
            {
              "operator": "gte",
              "key": "birthDate",
              "value": "19910101"
            },
            {
              "operator": "lte",
              "key": "birthDate",
              "value": "20010101"
            }
          ]
        },
        {
          "operator": "between",
          "key" : "company.employees",
          "values": [500, 5000],
          "options": {
            "negate": true
          }
        }
      ]
  },
  "options": {
    "sortOptions" : [
      {
        "key": "birthDate",
        "desc": false
      }
    ]
    "selections" : [
		"birthDate",
		"firstName",
		"lastName"
	]
  }
  
}' 'http://localhost:8080/persons/projection'
```
---  
## Links
- Presentation article (version 0.0.1) on [Medium](https://medium.com/@biagio.tozzi/advanced-and-dynamic-searching-with-spring-data-jpa-e322e0f40b4b)
