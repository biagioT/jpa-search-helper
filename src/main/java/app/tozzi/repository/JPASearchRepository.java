package app.tozzi.repository;

import app.tozzi.core.JPASearchCore;
import app.tozzi.model.input.JPASearchInput;
import app.tozzi.util.JPASearchUtils;
import app.tozzi.util.ReflectionUtils;
import io.micrometer.observation.annotation.Observed;
import jakarta.persistence.criteria.JoinType;
import lombok.NonNull;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.util.List;
import java.util.Map;

public interface JPASearchRepository<E> extends JpaSpecificationExecutor<E> {

    /**
     * Search by filters.
     *
     * @param filters
     * @param type
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default List<E> findAll(@NonNull Map<String, String> filters, @NonNull Class<?> type) {
        Specification<E> specification = JPASearchCore.specification(
                JPASearchUtils.toObject(filters, false, false).getFilter(),
                ReflectionUtils.getAllSearchableFields(type), null, null);
        return findAll(specification);
    }

    /**
     * Search by filters.
     *
     * @param input
     * @param type
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default List<E> findAll(@NonNull JPASearchInput input, @NonNull Class<?> type) {
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), ReflectionUtils.getAllSearchableFields(type), null, null);
        return findAll(specification);
    }

    /**
     * Search by filters with forced fetched Join.
     *
     * @param filters
     * @param type
     * @param fetches
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default List<E> findAll(@NonNull Map<String, String> filters, @NonNull Class<?> type, Map<String, JoinType> fetches) {
        Specification<E> specification = JPASearchCore.specification(
                JPASearchUtils.toObject(filters, false, false).getFilter(),
                ReflectionUtils.getAllSearchableFields(type), fetches, null);
        return findAll(specification);
    }

    /**
     * Search by filters with forced fetched Join.
     *
     * @param input
     * @param type
     * @param fetches
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default List<E> findAll(@NonNull JPASearchInput input, @NonNull Class<?> type, Map<String, JoinType> fetches) {
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), ReflectionUtils.getAllSearchableFields(type), fetches, null);
        return findAll(specification);
    }

    /**
     * Search by filters with forced fetched Join and a map of:
     * - key: bean/dto field name
     * - value: entity field name
     *
     * @param filters
     * @param type
     * @param fetches
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default List<E> findAll(@NonNull Map<String, String> filters, @NonNull Class<?> type, Map<String, JoinType> fetches, Map<String, String> entityFieldMap) {
        Specification<E> specification = JPASearchCore.specification(
                JPASearchUtils.toObject(filters, false, false).getFilter(),
                ReflectionUtils.getAllSearchableFields(type), fetches, entityFieldMap);
        return findAll(specification);
    }

    /**
     * Search by filters with forced fetched Join and a map of:
     * - key: bean/dto field name
     * - value: entity field name
     *
     * @param input
     * @param type
     * @param fetches
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default List<E> findAll(@NonNull JPASearchInput input, @NonNull Class<?> type, Map<String, JoinType> fetches, Map<String, String> entityFieldMap) {
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), ReflectionUtils.getAllSearchableFields(type), fetches, entityFieldMap);
        return findAll(specification);
    }

    /**
     * Search by filters.
     *
     * @param filters
     * @param type
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default List<E> findAllSorted(@NonNull Map<String, String> filters, @NonNull Class<?> type) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(type);
        var input = JPASearchUtils.toObject(filters, false, true);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, null, null);
        Sort sort = JPASearchCore.loadSort(input.getOptions(), searchableFields, null);
        return findAll(specification, sort);
    }

    /**
     * Search by input.
     *
     * @param input
     * @param type
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default List<E> findAllSorted(@NonNull JPASearchInput input, @NonNull Class<?> type) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(type);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, null, null);
        Sort sort = JPASearchCore.loadSort(input.getOptions(), searchableFields, null);
        return findAll(specification, sort);
    }

    /**
     * Search by filters with forced fetched Join.
     *
     * @param filters
     * @param type
     * @param fetches
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default List<E> findAllSorted(@NonNull Map<String, String> filters, @NonNull Class<?> type, Map<String, JoinType> fetches) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(type);
        var input = JPASearchUtils.toObject(filters, false, true);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, fetches, null);
        Sort sort = JPASearchCore.loadSort(input.getOptions(), searchableFields, null);
        return findAll(specification, sort);
    }

    /**
     * Search by filters with forced fetched Join.
     *
     * @param input
     * @param type
     * @param fetches
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default List<E> findAllSorted(@NonNull JPASearchInput input, @NonNull Class<?> type, Map<String, JoinType> fetches) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(type);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, fetches, null);
        Sort sort = JPASearchCore.loadSort(input.getOptions(), searchableFields, null);
        return findAll(specification, sort);
    }

    /**
     * Search by filters with forced fetched Join and a map of:
     * - key: bean/dto field name
     * - value: entity field name
     *
     * @param filters
     * @param type
     * @param fetches
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default List<E> findAllSorted(@NonNull Map<String, String> filters, @NonNull Class<?> type, Map<String, JoinType> fetches, Map<String, String> entityFieldMap) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(type);
        var input = JPASearchUtils.toObject(filters, false, true);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, fetches, entityFieldMap);
        Sort sort = JPASearchCore.loadSort(input.getOptions(), searchableFields, entityFieldMap);
        return findAll(specification, sort);
    }

    /**
     * Search by filters with forced fetched Join and a map of:
     * - key: bean/dto field name
     * - value: entity field name
     *
     * @param input
     * @param type
     * @param fetches
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default List<E> findAllSorted(@NonNull JPASearchInput input, @NonNull Class<?> type, Map<String, JoinType> fetches, Map<String, String> entityFieldMap) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(type);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, fetches, entityFieldMap);
        Sort sort = JPASearchCore.loadSort(input.getOptions(), searchableFields, entityFieldMap);
        return findAll(specification, sort);
    }

    /**
     * Paginated search
     *
     * @param filters
     * @param type
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default Page<E> findAllWithPaginationAndSorting(@NonNull Map<String, String> filters, @NonNull Class<?> type) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(type);
        var input = JPASearchUtils.toObject(filters, true, true);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, null, null);
        PageRequest pageRequest = JPASearchCore.loadSortAndPagination(input.getOptions(), searchableFields, null);
        return findAll(specification, pageRequest);
    }

    /**
     * Paginated search
     *
     * @param input
     * @param type
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default Page<E> findAllWithPaginationAndSorting(@NonNull JPASearchInput input, @NonNull Class<?> type) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(type);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, null, null);
        PageRequest pageRequest = JPASearchCore.loadSortAndPagination(input.getOptions(), searchableFields, null);
        return findAll(specification, pageRequest);
    }

    /**
     * Paginated search with a map of:
     * - key: bean/dto field name
     * - value: entity field name
     *
     * @param filters
     * @param type
     * @param entityFieldMap
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default Page<E> findAllWithPaginationAndSorting(@NonNull Map<String, String> filters, @NonNull Class<?> type, Map<String, String> entityFieldMap) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(type);
        var input = JPASearchUtils.toObject(filters, true, true);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, null, entityFieldMap);
        PageRequest pageRequest = JPASearchCore.loadSortAndPagination(input.getOptions(), searchableFields, entityFieldMap);
        return findAll(specification, pageRequest);
    }

    /**
     * Paginated search with a map of:
     * - key: bean/dto field name
     * - value: entity field name
     *
     * @param input
     * @param type
     * @param entityFieldMap
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default Page<E> findAllWithPaginationAndSorting(@NonNull JPASearchInput input, @NonNull Class<?> type, Map<String, String> entityFieldMap) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(type);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, null, entityFieldMap);
        PageRequest pageRequest = JPASearchCore.loadSortAndPagination(input.getOptions(), searchableFields, entityFieldMap);
        return findAll(specification, pageRequest);
    }

    /**
     * Count
     *
     * @param filters
     * @param type
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default long count(@NonNull Map<String, String> filters, @NonNull Class<?> type) {
        Specification<E> specification = JPASearchCore.specification(JPASearchUtils.toObject(filters, false, false).getFilter(),
                ReflectionUtils.getAllSearchableFields(type), null, null);
        return count(specification);
    }

    /**
     * Count
     *
     * @param input
     * @param type
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default long count(JPASearchInput input, @NonNull Class<?> type) {
        Specification<E> specification = JPASearchCore.specification(input.getFilter(),
                ReflectionUtils.getAllSearchableFields(type), null, null);
        return count(specification);
    }

    /**
     * Count
     *
     * @param filters
     * @param type
     * @param entityFieldMap
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default long count(@NonNull Map<String, String> filters, @NonNull Class<?> type, Map<String, String> entityFieldMap) {
        Specification<E> specification = JPASearchCore.specification(JPASearchUtils.toObject(filters, false, false).getFilter(),
                ReflectionUtils.getAllSearchableFields(type), null, entityFieldMap);
        return count(specification);
    }

    /**
     * Count
     *
     * @param input
     * @param type
     * @param entityFieldMap
     * @return
     */
    @Observed(contextualName = "jpa-search-repository")
    default long count(JPASearchInput input, @NonNull Class<?> type, Map<String, String> entityFieldMap) {
        Specification<E> specification = JPASearchCore.specification(input.getFilter(),
                ReflectionUtils.getAllSearchableFields(type), null, entityFieldMap);
        return count(specification);
    }

}

