package app.tozzi.repository;

import app.tozzi.model.JPAProjectionOptions;
import app.tozzi.model.input.JPASearchInput;
import lombok.NonNull;

import java.util.List;
import java.util.Map;

/**
 * Unified projection API exposed by the library.
 * <p>
 * Each operation comes in four flavours:
 * <ul>
 *     <li>{@link JPASearchInput} (Mode 2 / JSON) with and without {@link JPAProjectionOptions}</li>
 *     <li>{@code Map<String, String>} (Mode 1 / query params) with and without {@link JPAProjectionOptions}</li>
 * </ul>
 * The {@link JPAProjectionOptions} parameter object aggregates {@code fetches}, {@code entityFieldMap}
 * and {@code overrideJoinTypes} (the so-called <em>classic</em> mode), replacing the many overloads
 * that existed before release 4.0.
 *
 * @param <E> entity type
 * @since 4.0.0
 */
public interface JPAProjectionRepository<E> {

    // ---------- projection ----------

    List<Map<String, Object>> projection(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass);

    List<Map<String, Object>> projection(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass);

    List<Map<String, Object>> projection(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, @NonNull JPAProjectionOptions options);

    List<Map<String, Object>> projection(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, @NonNull JPAProjectionOptions options);

    // ---------- projectionWithSorting ----------

    List<Map<String, Object>> projectionWithSorting(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass);

    List<Map<String, Object>> projectionWithSorting(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass);

    List<Map<String, Object>> projectionWithSorting(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, @NonNull JPAProjectionOptions options);

    List<Map<String, Object>> projectionWithSorting(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, @NonNull JPAProjectionOptions options);
}

