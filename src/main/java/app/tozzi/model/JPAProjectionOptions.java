package app.tozzi.model;

import jakarta.persistence.criteria.JoinType;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.Map;

/**
 * Aggregates the optional parameters used by the projection operations exposed by
 * {@link app.tozzi.repository.JPAProjectionRepository}.
 * <p>
 * Use {@link #builder()} to create an instance, or {@link #empty()} when no extra options are needed.
 * <p>
 * If {@link Builder#useOverrideJoinTypes(boolean)} is set to {@code true} (the so-called "Classic" mode),
 * the projection will not force LEFT joins on the navigated relationships and will instead build only the
 * joins listed in {@link Builder#overrideJoinTypes(Map)} (with the requested {@link JoinType}).
 *
 * @since 4.0.0
 */
@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class JPAProjectionOptions {

    private static final JPAProjectionOptions EMPTY = new JPAProjectionOptions();

    private Map<String, JoinType> fetches;
    private Map<String, String> entityFieldMap;
    private boolean useOverrideJoinTypes;
    private Map<String, JoinType> overrideJoinTypes;

    public static JPAProjectionOptions empty() {
        return EMPTY;
    }

    public static Builder builder() {
        return new Builder();
    }

    public static class Builder {

        private Map<String, JoinType> fetches;
        private Map<String, String> entityFieldMap;
        private boolean useOverrideJoinTypes;
        private Map<String, JoinType> overrideJoinTypes;

        public Builder fetches(Map<String, JoinType> fetches) {
            this.fetches = fetches;
            return this;
        }

        public Builder entityFieldMap(Map<String, String> entityFieldMap) {
            this.entityFieldMap = entityFieldMap;
            return this;
        }

        /**
         * When set to {@code true}, disables the default behaviour of forcing LEFT joins on every
         * navigated relationship: the projection will instead build only the joins listed in
         * {@link #overrideJoinTypes(Map)} (with the configured {@link JoinType}). This was the
         * behaviour exposed by the {@code projectionClassic}/{@code projectionWithSortingClassic}
         * methods of the legacy API removed in release 4.0.
         */
        public Builder useOverrideJoinTypes(boolean useOverrideJoinTypes) {
            this.useOverrideJoinTypes = useOverrideJoinTypes;
            return this;
        }

        public Builder overrideJoinTypes(Map<String, JoinType> overrideJoinTypes) {
            this.overrideJoinTypes = overrideJoinTypes;
            this.useOverrideJoinTypes = true;
            return this;
        }

        public JPAProjectionOptions build() {
            return new JPAProjectionOptions(fetches, entityFieldMap, useOverrideJoinTypes, overrideJoinTypes);
        }
    }
}

