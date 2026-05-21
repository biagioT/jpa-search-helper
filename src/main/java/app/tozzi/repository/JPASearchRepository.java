package app.tozzi.repository;

import app.tozzi.core.JPASearchCore;
import app.tozzi.model.JPASearchOptions;
import app.tozzi.model.input.JPASearchInput;
import app.tozzi.util.JPASearchUtils;
import app.tozzi.util.ReflectionUtils;
import io.micrometer.observation.annotation.Observed;
import lombok.NonNull;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Slice;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.util.List;
import java.util.Map;

/**
 * Unified search API exposed by the library.
 * <p>
 * Every operation comes in four flavours:
 * <ul>
 *     <li>{@link JPASearchInput} (Mode 2 / JSON) with and without {@link JPASearchOptions}</li>
 *     <li>{@code Map<String, String>} (Mode 1 / query params) with and without {@link JPASearchOptions}</li>
 * </ul>
 * The {@link JPASearchOptions} parameter object aggregates {@code fetches} and {@code entityFieldMap},
 * replacing the many overloads that existed before release 4.0.
 *
 * @param <E> entity type
 * @since 4.0.0
 */
@Observed(contextualName = "jpa-search-repository")
public interface JPASearchRepository<E> extends JpaSpecificationExecutor<E> {

    // ---------- findAll ----------

    default List<E> findAll(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType) {
        return findAll(input, domainModelOrEntityType, JPASearchOptions.empty());
    }

    default List<E> findAll(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType) {
        return findAll(filters, domainModelOrEntityType, JPASearchOptions.empty());
    }

    default List<E> findAll(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull JPASearchOptions options) {
        var fields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        Specification<E> spec = JPASearchCore.specification(input.getFilter(), fields, options.getFetches(), options.getEntityFieldMap());
        return findAll(spec);
    }

    default List<E> findAll(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull JPASearchOptions options) {
        return findAll(JPASearchUtils.toObject(filters, false, false, false), domainModelOrEntityType, options);
    }

    // ---------- findAllSorted ----------

    default List<E> findAllSorted(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType) {
        return findAllSorted(input, domainModelOrEntityType, JPASearchOptions.empty());
    }

    default List<E> findAllSorted(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType) {
        return findAllSorted(filters, domainModelOrEntityType, JPASearchOptions.empty());
    }

    default List<E> findAllSorted(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull JPASearchOptions options) {
        var fields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        Specification<E> spec = JPASearchCore.specification(input.getFilter(), fields, options.getFetches(), options.getEntityFieldMap());
        var sort = JPASearchCore.loadSort(input.getOptions(), fields, options.getEntityFieldMap());
        return findAll(spec, sort);
    }

    default List<E> findAllSorted(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull JPASearchOptions options) {
        return findAllSorted(JPASearchUtils.toObject(filters, false, true, false), domainModelOrEntityType, options);
    }

    // ---------- findAllWithPaginationAndSorting (Page) ----------

    default Page<E> findAllWithPaginationAndSorting(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType) {
        return findAllWithPaginationAndSorting(input, domainModelOrEntityType, JPASearchOptions.empty());
    }

    default Page<E> findAllWithPaginationAndSorting(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType) {
        return findAllWithPaginationAndSorting(filters, domainModelOrEntityType, JPASearchOptions.empty());
    }

    default Page<E> findAllWithPaginationAndSorting(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull JPASearchOptions options) {
        var fields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        Specification<E> spec = JPASearchCore.specification(input.getFilter(), fields, options.getFetches(), options.getEntityFieldMap());
        var pageRequest = JPASearchCore.loadSortAndPagination(input.getOptions(), fields, options.getEntityFieldMap());
        return findAll(spec, pageRequest);
    }

    default Page<E> findAllWithPaginationAndSorting(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull JPASearchOptions options) {
        return findAllWithPaginationAndSorting(JPASearchUtils.toObject(filters, true, true, false), domainModelOrEntityType, options);
    }

    // ---------- findAllWithPaginationAndSortingLazy (Slice) ----------

    default Slice<E> findAllWithPaginationAndSortingLazy(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType) {
        return findAllWithPaginationAndSortingLazy(input, domainModelOrEntityType, JPASearchOptions.empty());
    }

    default Slice<E> findAllWithPaginationAndSortingLazy(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType) {
        return findAllWithPaginationAndSortingLazy(filters, domainModelOrEntityType, JPASearchOptions.empty());
    }

    default Slice<E> findAllWithPaginationAndSortingLazy(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull JPASearchOptions options) {
        var fields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        Specification<E> spec = JPASearchCore.specification(input.getFilter(), fields, options.getFetches(), options.getEntityFieldMap());
        var pageRequest = JPASearchCore.loadSortAndPagination(input.getOptions(), fields, options.getEntityFieldMap());
        return findAll(spec, pageRequest);
    }

    default Slice<E> findAllWithPaginationAndSortingLazy(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull JPASearchOptions options) {
        return findAllWithPaginationAndSortingLazy(JPASearchUtils.toObject(filters, true, true, false), domainModelOrEntityType, options);
    }

    // ---------- count ----------

    default long count(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType) {
        return count(input, domainModelOrEntityType, JPASearchOptions.empty());
    }

    default long count(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType) {
        return count(filters, domainModelOrEntityType, JPASearchOptions.empty());
    }

    default long count(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull JPASearchOptions options) {
        Specification<E> spec = JPASearchCore.specification(input.getFilter(),
                ReflectionUtils.getAllSearchableFields(domainModelOrEntityType), options.getFetches(), options.getEntityFieldMap());
        return count(spec);
    }

    default long count(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull JPASearchOptions options) {
        return count(JPASearchUtils.toObject(filters, false, false, false), domainModelOrEntityType, options);
    }
}

