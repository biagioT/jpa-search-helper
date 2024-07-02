# JPA Search Helper
Library for building and running advanced and dynamic queries using JPA in Spring Boot.

## TL;DR
The library offers two ways to build queries:
-  **Mode 1**: Via Map<String, String>, to support GET endpoints with query params
-  **Mode 2**: Via an object (easily serializable/deserializable via JSON), to support POST endpoints that expect query parameters in the body (new from 2.0.0 version)

### Spoiler
Through **jpa-search-helper** your controller will be able to receive requests like this:

**Mode 1**:
```bash
curl -X GET \
  'https://myexampledomain.com/persons?
  firstName=Biagio
  &lastName_startsWith=Toz
  &birthDate_gte=19910101
  &country_in=IT,FR,DE
  &address_eq=Via Roma 1,Via Milano\,1,20 West\,34th Street
  &company.name_in=Microsoft,Apple,Google
  &company.employees_between=500,5000'
```
**Mode 2**:

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
    "sortKey": "birthDate",
    "sortDesc": false
  }
  
}' 'https://myexampledomain.com/persons'
```

.. how you do it? Read this readme!

## Prerequisites
- Java 17 or later
- Spring Boot 3.2.x or later

## Project dependency
#### Maven
```
<dependency>
    <groupId>app.tozzi</groupId>
    <artifactId>jpa-search-helper</artifactId>
    <version>2.0.4</version>
</dependency>
```

#### Gradle
```
implementation 'app.tozzi:jpa-search-helper:2.0.4'
```

## Usage
### 1. `@Searchable` annotation
Start by applying the `@Searchable` annotation to the fields in your DTO/Domain model, or alternatively your JPA entity, **that you want to make available for search**.
```java
@Data
public class PersonDTO {

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
  public static class CompanyDTO {

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
  - `targetType`: the managed object type by entity. If not specified the librariy tries to obtain it based on field type (es. Integer field without target type definition will be INTEGER). If there is no type compatible with those managed, it will be managed as a string. Managed types:

    - `STRING`, `INTEGER`, `DOUBLE`, `FLOAT`, `LONG`, `BIGDECIMAL`, `BOOLEAN`, `DATE`, `LOCALDATE`, `LOCALDATETIME`, `LOCALTIME`, `OFFSETDATETIME`, `OFFSETTIME`, `ZONEDDATETIME`.

- Validation properties:
  
  - `datePattern`: only for `DATE`, `LOCALDATE`, `LOCALDATETIME`, `LOCALTIME`, `OFFSETDATETIME`, `OFFSETTIME`, `ZONEDDATETIME` target types. Defines the date pattern to use.
  - `maxSize, minSize`: maximum/minimum length of the value.
  - `maxDigits, minDigits`: only for numeric types. Maximum/minimum number of digits.
  - `regexPattern`: regex pattern.
  - `decimalFormat`: only for decimal numeric types. Default `#.##`

- Other:
  - `sortable`: if false, the field can be used by search but cannot be used for sorting. Default: true.
  - `trim`: apply trim.
  - `tags`: useful if the DTO field can correspond to multiple entity fields (the example is available further down).
  - `allowedFilters`: exclusively allowed filters.
  - `notAllowedFilters`: not allowed filters.
  - `likeFilters`: allowed like filters (_contains_, _startsWith_, _endsWith_). Default: true.

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
Your Spring JPA repository must extend JPASearchRepository< YourEntityClass >.

```java
@Repository
public interface PersonRepository extends JpaRepository<PersonEntity, Long>, JPASearchRepository<PersonEntity> {

}
```

### 3. Search implementation
In your manager, or in your service, or wherever you want to use the repository:

**Mode 1**: define a map _<filter_key|options|, value>_:
```java
// ...

  @Autowired
  private PersonRepository personRepository;

    // ...

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
    List<PersonEntity> fullSearch = personRepository.findAll(filters, PersonDTO.class);
  
    filters.put("birthDate_sort" : "ASC"); // sorting key and sorting order
    filters.put("_limit", "10"); // page size
    filters.put("_offset", "0"); // page offset
    
    // With pagination
    Page<PersonEntity> sortedAndPaginatedSearch = personRepository.findAllWithPaginationAndSorting(filters, PersonDTO.class);
    
    // ...

// ...
```

**Mode 2**: you will need to use `JPASearchInput`, which I will show here, for simplicity, in JSON format:
Through mode 2 it is possible to manage complex filters with AND, OR and NOT (see later).
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
    "sortKey": "birthDate",
    "sortDesc": false
  }
  
}
```

#### Exceptions:
- If a field does not exist, is not searchable or is not sortable, you will receive an `InvalidFieldException`.
- If the value of a field does not meet the requirements you will receive an `InvalidValueException`.
- Other cases: `JPASearchException`

## Docs
### Managed root operators
| Filter name | Library Key | Supported modes |
|-------------|-------------|-----------------|
| AND         | and         | 1, 2            |
| OR          | or          | 2               |
| NOT         | not         | 2               |

Through Mode 1, all filters compose exclusively an _AND_ search.

To use the other operators, _OR_ and _NOT_, you must use Mode 2

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
| Empty                     | empty       | sql_coll_col IS NULL               | 1,2 | no             |

#### Options
**Mode 1**: the option keys must be appended to the filter; e.g. _?firstName_eq#i=Biagio_ or _?firstName_eq#i#n=Biagio_

| Option description | Library Key |
|--------------------|-------------|
| Ignore case        | #i          |
| Negation           | #n          |
| Trim               | #t          |

**Mode 2**:

| Option description | Library Key (Java attributes) |
|------------------|-------------------------------|
| Ignore case      | ignoreCase                    |
| Negation      | negate                        |
| Trim | trim                          |

For each filter, value the `JPASearchFilterOptions` attributes
```java
@Data
public static class JPASearchFilterOptions {
    private boolean ignoreCase;
    private boolean trim;
    private boolean negate;
}
```
or if you prefer to manage a JSON:
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

### Pagination

| Filter name | Key | Fixed values
|--|--|--|
| Limit (page size) | limit
| Offset (page number) | offset
| Sort | sort | ASC, DESC

**Mode 1**: e.g. ?firstName_sort=DESC&_limit=10&_offset=1

**Mode 2**: value the `JPASearchOptions` attributes
```java
@Data
public static class JPASearchOptions {
    private String sortKey;
    private Boolean sortDesc = false;
    private Integer pageSize;
    private Integer pageOffset;
}
```
or if you prefer to manage a JSON:
```json
{
  "filter" : {
    // ...
  },
  "options" : {
    "sortKey": "firstName",
    "sortDesc": true,
    "pageSize": 10,
    "pageOffset": 1
  }
}
```

### Other (only for Mode 1)
- Separator for array values: `,`; e.g. _?myField_in=test1,test2_ --> values: ["test1", "test2"]
- To escape separator: `/,`; e.g. _?myField_in=test1,test2/,test3_ --> values: ["test1", "test2,test3"]


---
### Spring Boot Project example with HTTP Endpoint
Note: This library does not expose any endpoints and therefore no controllers.

An example project is available [here](https://github.com/biagioT/jpa-search-helper-demo).

#### Mode 1

Controller:
```java
@RestController
public class MyController {

  @Autowired
  private PersonManager personManager;

  @GetMapping(path="/persons", produces = MediaType.APPLICATION_JSON_VALUE)
  public List<PersonDTO> findPersons(@RequestParam Map<String, String> requestParams) {
    return personManager.find(requestParams);
  }
}
```

Service/Manager bean:
```java
@Service
public class PersonManager {

  @Autowired
  private PersonRepository personRepository;

  public List<PersonDTO> find(Map<String, String> filters) {
    return personRepository.findAllWithPaginationAndSorting(filters, PersonDTO.class).stream().map(this::toDTO).toList();
  }

  private static PersonDTO toDTO(PersonEntity personEntity) {
    // ...
  }

}
```
Curl:
```bash
curl - X GET \
'http://localhost:8080/persons?
firstName=Biagio
&lastName_startsWith=Toz
&birthDate_gte=19910101
&country_in=IT,FR,DE
&company.name_in=Microsoft,Apple
&company.employees_between=500,5000'
```

#### Mode 2

Controller:
```java
@RestController
public class MyController {

  @Autowired
  private PersonManager personManager;

  @PostMapping(path="/persons", produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public List<PersonDTO> findPersons(@RequestBody JPASearchInput input) {
    return personManager.find(input);
  }
}
```

Service/Manager bean:
```java
@Service
public class PersonManager {

  @Autowired
  private PersonRepository personRepository;

  public List<PersonDTO> find(JPASearchInput input) {
    return personRepository.findAllWithPaginationAndSorting(input, PersonDTO.class).stream().map(this::toDTO).toList();
  }

  private static PersonDTO toDTO(PersonEntity personEntity) {
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
    "sortKey": "birthDate",
    "sortDesc": false
  }
  
}' 'http://localhost:8080/persons'
```

### Join Fetch
It is possible to force joins with fetch to allow Hibernate to execute a single query for the relationships defined on the entity. **This is only possible without pagination**:
```java
// ...

Map<String, JoinFetch> fetches = Map.of("companyEntity", JoinFetch.LEFT);
personRepository.findAll(filters, PersonDTO.class, fetches);

// ...
```

### Multiple entities for the same DTO
If you have a DTO that is the result of the conversion of multiple entities, it is possible to explicitly specify a map (string, string) whose key represents the name of the DTO field and the value is the name of the field of the entity to be searched for:
```java
// ...

Map<String, String> entityFieldMap = Map.of("company", "companyEntity.name");

// Without pagination
personRepository.findAll(filters, PersonDTO.class, fetches, entityFieldMap);

// With pagination
personRepository.findAllWithPaginationAndSorting(filters, PersonDTO.class, entityFieldMap);

// ...
```

### Multiple object for the same entity
Another special case could be where an object can be repeated within the DTO to represent multiple pieces of the entity. The solution for the search:
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
public class CoupleDTO {

  @NestedSearchable
  private PersonDTO p1;

  @NestedSearchable
  private PersonDTO p2;

  @Data
  public static class PersonDTO {

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
curl - request GET \
 - url 'https://www.myexampledomain.com/couples?
p1.firstName_iEq=Romeo
&p2.firstName_iEq=Giulietta'
```
