# Changelog

All notable changes to **JPA Search Helper** will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [4.1.0] - 2026-06-12

> Up to Spring Boot 4.1.0 compatibility, with no breaking changes.

## [4.0.0] - 2026-05-21

> ⚠️ **Major release with breaking changes.** The repository APIs have been redesigned
> around two parameter objects (`JPASearchOptions` and `JPAProjectionOptions`); the many
> overloads accepting `fetches` / `entityFieldMap` / `overrideJoinTypes` directly as method
> parameters have been removed. Migration is mechanical — see the *Migration Guide* below.

### ⚠ Breaking Changes

- **`JPASearchRepository`**: removed all overloads that took `Map<String, JoinType> fetches`
  and/or `Map<String, String> entityFieldMap` as separate parameters. Each operation
  (`findAll`, `findAllSorted`, `findAllWithPaginationAndSorting`,
  `findAllWithPaginationAndSortingLazy`, `count`) now exposes only **four** signatures:
  `(input, type)`, `(filters, type)`, `(input, type, options)`, `(filters, type, options)`.
- **`JPAProjectionRepository`**: removed all overloads with separate `fetches`,
  `entityFieldMap`, `overrideJoinTypes` parameters. Removed entirely the
  `projectionClassic` and `projectionWithSortingClassic` methods — their behaviour is
  now exposed through `JPAProjectionOptions.builder().useOverrideJoinTypes(true)` (and
  optionally `.overrideJoinTypes(...)`).
- The default behaviour of forcing `LEFT JOIN` on navigated relationships of a projection
  is unchanged when using the new API without setting `useOverrideJoinTypes`.

### ✨ Added

- **`JPASearchOptions`** parameter object (with fluent `builder()` and `empty()` factory)
  to consolidate `fetches` and `entityFieldMap`.
- **`JPAProjectionOptions`** parameter object (with fluent `builder()` and `empty()`
  factory) to consolidate `fetches`, `entityFieldMap`, `useOverrideJoinTypes`,
  `overrideJoinTypes`.
- **Spring Boot auto-configuration** (`JPASearchAutoConfiguration`): consumers no longer
  need to declare an explicit `@ComponentScan("app.tozzi")` /
  `@EnableJpaRepositories("app.tozzi.repository")` block to register
  `JPAProjectionRepositoryImpl`. Registration is conditional on Spring Boot being on the
  classpath and on no other bean of the same type being defined.
- **`elementCollection` + `EMPTY` / `NULL` operators**: dedicated branch in
  `JPASearchCore` that uses `cb.isEmpty(collectionPath)` instead of falling through to a
  `LEFT JOIN` + `LIKE`/`IS NULL` chain that did not produce correct SQL.
- New unit tests dedicated to the 4.0 fixes
  (`JPASearchCoreFixes4Test`, `JPASearchCoreValueProcessorNumberParsingTest`,
  `JPASearchCoreValueProcessorCollectionEnumTest`, plus assertions added in
  `JPAProjectionProcessorTest`, `JPASearchCoreFieldProcessorTest`, `JPASearchCoreTest`,
  `ProjectionBugVerificationTest`).

### 🐛 Fixed

- **`DISTINCT` no longer applied unconditionally**: `JPASearchCore.specification` now
  sets `query.distinct(true)` only when joins or fetches are actually present on the
  root, removing the (small but unnecessary) overhead on simple queries that have no
  cartesian-product risk.
- **`NULL` / `EMPTY` on nested optional relationships now uses `LEFT JOIN`**: previously
  the implicit chain `root.get("a").get("b")` produced an inner join that silently
  filtered out rows where the relation was `null`, defeating the `IS NULL` predicate.
  A new `buildLeftJoinPath` helper in `JPASearchCore` constructs explicit `LEFT JOIN`s
  on intermediate path segments (with reuse of already-existing joins).
- **`JPAProjectionProcessor.toMap` no longer collapses distinct compound-id entities**:
  the cache key was previously built by sorting the raw id values by `Object::toString`,
  causing entities whose compound keys were value-permutations of each other (e.g.
  `(a=1, b=2)` and `(a=2, b=1)`) to collide on the same key. Sorting is now done by
  id-field name, preserving identity.
- **`JPASearchCoreValueProcessor.getSize`**: extended to handle `String` values for
  `FLOAT` / `DOUBLE` / `BIGDECIMAL` types (regression that surfaced after the
  `containsOnlyDigits` regex was broadened).
- Misleading or imprecise error messages corrected in `JPASearchCoreFieldProcessor`
  and `JPASearchCoreValueProcessor` (missing brackets, sortable/tag-related cases).
- `RootFilter.filters` is no longer marked `@NotEmpty` in `JPASearchInput`: it is
  legitimate to send a request with no filters.

### 🚀 Improved

- `GenericUtils#containsOnlyDigits` broadened from `\d+` to `-?\d+(\.\d+)?` to recognize
  signed and decimal numeric strings.
- `DateTimeFormatter` instances are now cached in a static `ConcurrentHashMap`, avoiding
  the repeated `DateTimeFormatter.ofPattern(...)` cost on hot search paths
  (`parseLocalDate`, `parseLocalTime`, `parseLocalDateTime`, `parseOffsetTime`,
  `parseOffsetDateTime`, `parseZonedDateTime`, `parseInstant`).
- `ValidationUtils` cleaned up: removed superfluous null checks on non-nullable
  annotation array returns, error messages normalised to singular form.
- `Collections.EMPTY_LIST` replaced with `Collections.emptyList()` where present.

### 📚 Documentation

- New JavaDoc on `JPASearchType.UNTYPED` clarifying it is an internal sentinel.
- New JavaDoc on `Searchable.targetType()` and `Searchable.elementCollection()`
  describing the EQ/IN `cb.isMember(...)` path and the `LEFT JOIN` fallback for the
  other operators.
- `README.md` overhauled:
  - compatibility table updated for 4.x;
  - Maven/Gradle coordinates bumped to 4.0.0;
  - `Advanced Settings` section rewritten around `JPASearchOptions` /
    `JPAProjectionOptions`;
  - `elementCollection` section added;
  - clarified `maxSize` / `minSize` semantics for `STRING`;
  - replaced the old `⚠️ Important Configuration Note` with the new auto-configuration
    note;
  - fixed several typos, broken Java/JSON snippets, and table inconsistencies
    (e.g. `IS NULL` → `IS EMPTY`, missing UUID in the managed types list).

### 🔧 Build

- `org.springframework.boot:spring-boot-autoconfigure` added as `compileOnly` (only
  brought in transitively when the consumer is itself a Spring Boot application).
- `META-INF/spring/org.springframework.boot.autoconfigure.AutoConfiguration.imports`
  registered to enable the new auto-configuration on Spring Boot 3.0+.

### 📦 Migration Guide (3.6.x → 4.0.0)

Replace the old multi-overload calls with the `JPASearchOptions` /
`JPAProjectionOptions` parameter objects:

```java
// 3.6.x
personRepository.findAll(filters, Person.class, fetches, entityFieldMap);

// 4.0.0
var options = JPASearchOptions.builder()
        .fetches(fetches)
        .entityFieldMap(entityFieldMap)
        .build();
personRepository.findAll(filters, Person.class, options);
```

```java
// 3.6.x — projection in "classic" mode (no forced LEFT JOIN)
personRepository.projectionClassic(filters, PersonModel.class, Person.class, overrideJoinTypes);

// 4.0.0 — same semantics via the unified API
var options = JPAProjectionOptions.builder()
        .useOverrideJoinTypes(true)
        .overrideJoinTypes(overrideJoinTypes)
        .build();
personRepository.projection(filters, PersonModel.class, Person.class, options);
```

Spring Boot consumers can also drop the explicit
`@ComponentScan("app.tozzi") @EnableJpaRepositories("app.tozzi.repository")` boilerplate
that the README used to recommend in the *Spring Configuration* section: the auto-
configuration now takes care of it.

---

## [3.6.4] and earlier

See the [GitHub Releases page](https://github.com/biagioT/jpa-search-helper/releases)
for the history prior to 4.0.

[4.0.0]: https://github.com/biagioT/jpa-search-helper/releases/tag/v4.0.0

