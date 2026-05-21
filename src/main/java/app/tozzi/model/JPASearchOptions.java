package app.tozzi.model;

import jakarta.persistence.criteria.JoinType;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.Map;

/**
 * Aggregates the optional parameters used by the search operations exposed by
 * {@link app.tozzi.repository.JPASearchRepository}.
 * <p>
 * Use {@link #builder()} to create an instance, or {@link #empty()} when no extra options are needed:
 * <pre>{@code
 *   var options = JPASearchOptions.builder()
 *           .fetches(Map.of("companyEntity", JoinType.LEFT))
 *           .entityFieldMap(Map.of("company", "companyEntity.name"))
 *           .build();
 *
 *   repository.findAll(input, Person.class, options);
 * }</pre>
 * <p>
 * The parameter object exists to consolidate the many overloads accepting {@code fetches} and
 * {@code entityFieldMap} into a single, easy-to-extend signature.
 *
 * @since 4.0.0
 */
@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class JPASearchOptions {

    private static final JPASearchOptions EMPTY = new JPASearchOptions();

    private Map<String, JoinType> fetches;
    private Map<String, String> entityFieldMap;

    public static JPASearchOptions empty() {
        return EMPTY;
    }

    public static Builder builder() {
        return new Builder();
    }

    public static class Builder {

        private Map<String, JoinType> fetches;
        private Map<String, String> entityFieldMap;

        public Builder fetches(Map<String, JoinType> fetches) {
            this.fetches = fetches;
            return this;
        }

        public Builder entityFieldMap(Map<String, String> entityFieldMap) {
            this.entityFieldMap = entityFieldMap;
            return this;
        }

        public JPASearchOptions build() {
            return new JPASearchOptions(fetches, entityFieldMap);
        }
    }
}

