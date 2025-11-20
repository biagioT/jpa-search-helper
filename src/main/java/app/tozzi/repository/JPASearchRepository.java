package app.tozzi.repository;

import app.tozzi.core.JPASearchCore;
import app.tozzi.model.input.JPASearchInput;
import app.tozzi.util.JPASearchUtils;
import app.tozzi.util.ReflectionUtils;
import io.micrometer.observation.annotation.Observed;
import jakarta.persistence.criteria.JoinType;
import lombok.NonNull;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Slice;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.util.List;
import java.util.Map;

@Observed(contextualName = "jpa-search-repository")
public interface JPASearchRepository<E> extends JpaSpecificationExecutor<E> {

    /**
     * Mode 1: Search by filters without sorting and pagination
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @return list of entities
     */
    default List<E> findAll(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType) {
        Specification<E> specification = JPASearchCore.specification(
                JPASearchUtils.toObject(filters, false, false, false).getFilter(),
                ReflectionUtils.getAllSearchableFields(domainModelOrEntityType), null, null);
        return findAll(specification);
    }

    /**
     * Mode 2: Search by filters without sorting and pagination
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @return list of entities
     */
    default List<E> findAll(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType) {
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), ReflectionUtils.getAllSearchableFields(domainModelOrEntityType), null, null);
        return findAll(specification);
    }

    /**
     * Mode 1: Search by filters without sorting and pagination and with forced fetched Join.
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @param fetches                 fetches joins
     * @return list of entities
     */
    default List<E> findAll(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, Map<String, JoinType> fetches) {
        Specification<E> specification = JPASearchCore.specification(
                JPASearchUtils.toObject(filters, false, false, false).getFilter(),
                ReflectionUtils.getAllSearchableFields(domainModelOrEntityType), fetches, null);
        return findAll(specification);
    }

    /**
     * Mode 2: Search by filters without sorting and pagination and with forced fetched Join.
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @param fetches                 fetches joins
     * @return list of entities
     */
    default List<E> findAll(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, Map<String, JoinType> fetches) {
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), ReflectionUtils.getAllSearchableFields(domainModelOrEntityType), fetches, null);
        return findAll(specification);
    }

    /**
     * Mode 1: Search by filters without sorting and pagination, with forced fetched Join and with a map of:
     * <ul>
     * <li> key: domain object field name </li>
     * <li> value: entity field name </li>
     * </ul>
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @param fetches                 fetches joins
     * @param entityFieldMap
     * @return list of entities
     */
    default List<E> findAll(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, Map<String, JoinType> fetches, Map<String, String> entityFieldMap) {
        Specification<E> specification = JPASearchCore.specification(
                JPASearchUtils.toObject(filters, false, false, false).getFilter(),
                ReflectionUtils.getAllSearchableFields(domainModelOrEntityType), fetches, entityFieldMap);
        return findAll(specification);
    }

    /**
     * Mode 2: Search by filters without sorting and pagination, with forced fetched Join and with a map of:
     * <ul>
     * <li> key: domain object field name </li>
     * <li> value: entity field name </li>
     * </ul>
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @param fetches                 fetches joins
     * @param entityFieldMap
     * @return list of entities
     */
    default List<E> findAll(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, Map<String, JoinType> fetches, Map<String, String> entityFieldMap) {
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), ReflectionUtils.getAllSearchableFields(domainModelOrEntityType), fetches, entityFieldMap);
        return findAll(specification);
    }

    /**
     * Mode 1: Search by filters with sorting and without pagination
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @return list of entities
     */
    default List<E> findAllSorted(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var input = JPASearchUtils.toObject(filters, false, true, false);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, null, null);
        var sort = JPASearchCore.loadSort(input.getOptions(), searchableFields, null);
        return findAll(specification, sort);
    }

    /**
     * Mode 2: Search by filters with sorting and without pagination
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @return list of entities
     */
    default List<E> findAllSorted(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, null, null);
        var sort = JPASearchCore.loadSort(input.getOptions(), searchableFields, null);
        return findAll(specification, sort);
    }

    /**
     * Mode 1: Search by filters with sorting, without pagination and with forced fetched Join.
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @param fetches                 fetches joins
     * @return list of entities
     */
    default List<E> findAllSorted(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, Map<String, JoinType> fetches) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var input = JPASearchUtils.toObject(filters, false, true, false);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, fetches, null);
        var sort = JPASearchCore.loadSort(input.getOptions(), searchableFields, null);
        return findAll(specification, sort);
    }

    /**
     * Mode 2: Search by filters with sorting, without pagination and with forced fetched Join.
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @param fetches                 fetches joins
     * @return list of entities
     */
    default List<E> findAllSorted(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, Map<String, JoinType> fetches) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, fetches, null);
        var sort = JPASearchCore.loadSort(input.getOptions(), searchableFields, null);
        return findAll(specification, sort);
    }

    /**
     * Mode 1: Search by filters with sorting, without pagination, with forced fetched Join and with a map of:
     * <ul>
     * <li> key: domain object field name </li>
     * <li> value: entity field name </li>
     * </ul>
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the Search and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @param fetches                 fetches joins
     * @param entityFieldMap
     * @return list of entities
     */
    default List<E> findAllSorted(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, Map<String, JoinType> fetches, Map<String, String> entityFieldMap) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var input = JPASearchUtils.toObject(filters, false, true, false);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, fetches, entityFieldMap);
        var sort = JPASearchCore.loadSort(input.getOptions(), searchableFields, entityFieldMap);
        return findAll(specification, sort);
    }

    /**
     * Mode 2: Search by filters with sorting, without pagination, with forced fetched Join and with a map of:
     * <ul>
     * <li> key: domain object field name </li>
     * <li> value: entity field name </li>
     * </ul>
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @param fetches                 fetches joins
     * @param entityFieldMap
     * @return list of entities
     */
    default List<E> findAllSorted(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, Map<String, JoinType> fetches, Map<String, String> entityFieldMap) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, fetches, entityFieldMap);
        var sort = JPASearchCore.loadSort(input.getOptions(), searchableFields, entityFieldMap);
        return findAll(specification, sort);
    }

    /**
     * Mode 1: Search by filters with sorting and pagination
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @return list of entities of the searched page
     */
    default Page<E> findAllWithPaginationAndSorting(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var input = JPASearchUtils.toObject(filters, true, true, false);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, null, null);
        var pageRequest = JPASearchCore.loadSortAndPagination(input.getOptions(), searchableFields, null);
        return findAll(specification, pageRequest);
    }

    /**
     * Mode 1: Search by filters with sorting and pagination. Lazy mode: {@link Slice} instead of {@link Page}
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @return list of entities of the searched page
     */
    default Slice<E> findAllWithPaginationAndSortingLazy(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var input = JPASearchUtils.toObject(filters, true, true, false);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, null, null);
        var pageRequest = JPASearchCore.loadSortAndPagination(input.getOptions(), searchableFields, null);
        return findAll(specification, pageRequest);
    }

    /**
     * Mode 2: Search by filters with sorting and pagination
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @return list of entities of the searched page
     */
    default Page<E> findAllWithPaginationAndSorting(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, null, null);
        var pageRequest = JPASearchCore.loadSortAndPagination(input.getOptions(), searchableFields, null);
        return findAll(specification, pageRequest);
    }

    /**
     * Mode 2: Search by filters with sorting and pagination. Lazy mode: {@link Slice} instead of {@link Page}
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @return list of entities of the searched page
     */
    default Slice<E> findAllWithPaginationAndSortingLazy(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, null, null);
        var pageRequest = JPASearchCore.loadSortAndPagination(input.getOptions(), searchableFields, null);
        return findAll(specification, pageRequest);
    }

    /**
     * Mode 1: Search by filters with sorting, pagination and with a map of:
     * <ul>
     * <li> key: domain object field name </li>
     * <li> value: entity field name </li>
     * </ul>
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @param entityFieldMap
     * @return list of entities of the searched page
     */
    default Page<E> findAllWithPaginationAndSorting(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, Map<String, String> entityFieldMap) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var input = JPASearchUtils.toObject(filters, true, true, false);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, null, entityFieldMap);
        var pageRequest = JPASearchCore.loadSortAndPagination(input.getOptions(), searchableFields, entityFieldMap);
        return findAll(specification, pageRequest);
    }

    /**
     * Mode 1: Search by filters with sorting, pagination and with a map of:
     * <ul>
     * <li> key: domain object field name </li>
     * <li> value: entity field name </li>
     * </ul>
     * <p>
     * Lazy mode: {@link Slice} instead of {@link Page}
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @param entityFieldMap
     * @return list of entities of the searched page
     */
    default Slice<E> findAllWithPaginationAndSortingLazy(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, Map<String, String> entityFieldMap) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var input = JPASearchUtils.toObject(filters, true, true, false);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, null, entityFieldMap);
        var pageRequest = JPASearchCore.loadSortAndPagination(input.getOptions(), searchableFields, entityFieldMap);
        return findAll(specification, pageRequest);
    }

    /**
     * Mode 2: Search by filters with sorting, pagination and with a map of:
     * <ul>
     * <li> key: domain object field name </li>
     * <li> value: entity field name </li>
     * </ul>
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @param entityFieldMap
     * @return list of entities of the searched page
     */
    default Page<E> findAllWithPaginationAndSorting(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, Map<String, String> entityFieldMap) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, null, entityFieldMap);
        var pageRequest = JPASearchCore.loadSortAndPagination(input.getOptions(), searchableFields, entityFieldMap);
        return findAll(specification, pageRequest);
    }

    /**
     * Mode 2: Search by filters with sorting, pagination and with a map of:
     * <ul>
     * <li> key: domain object field name </li>
     * <li> value: entity field name </li>
     * </ul>
     *
     * Lazy mode: {@link Slice} instead of {@link Page}
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @param entityFieldMap
     * @return list of entities of the searched page
     */
    default Slice<E> findAllWithPaginationAndSortingLazy(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, Map<String, String> entityFieldMap) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        Specification<E> specification = JPASearchCore.specification(input.getFilter(), searchableFields, null, entityFieldMap);
        var pageRequest = JPASearchCore.loadSortAndPagination(input.getOptions(), searchableFields, entityFieldMap);
        return findAll(specification, pageRequest);
    }

    /**
     * Mode 1: Count
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @return number of results
     */
    default long count(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType) {
        Specification<E> specification = JPASearchCore.specification(JPASearchUtils.toObject(filters, false, false, false).getFilter(),
                ReflectionUtils.getAllSearchableFields(domainModelOrEntityType), null, null);
        return count(specification);
    }

    /**
     * Mode 2: Count
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @return number of results
     */
    default long count(JPASearchInput input, @NonNull Class<?> domainModelOrEntityType) {
        Specification<E> specification = JPASearchCore.specification(input.getFilter(),
                ReflectionUtils.getAllSearchableFields(domainModelOrEntityType), null, null);
        return count(specification);
    }

    /**
     * Mode 1: Count with a map of:
     * <ul>
     * <li> key: domain object field name </li>
     * <li> value: entity field name </li>
     * </ul>
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @param entityFieldMap
     * @return number of results
     */
    default long count(Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, Map<String, String> entityFieldMap) {
        Specification<E> specification = JPASearchCore.specification(JPASearchUtils.toObject(filters, false, false, false).getFilter(),
                ReflectionUtils.getAllSearchableFields(domainModelOrEntityType), null, entityFieldMap);
        return count(specification);
    }

    /**
     * Mode 2: Count with a map of:
     * <ul>
     * <li> key: domain object field name </li>
     * <li> value: entity field name </li>
     * </ul>
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Searchable} and {@link app.tozzi.annotation.NestedSearchable} annotations
     * @param entityFieldMap
     * @return number of results
     */
    default long count(JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, Map<String, String> entityFieldMap) {
        Specification<E> specification = JPASearchCore.specification(input.getFilter(),
                ReflectionUtils.getAllSearchableFields(domainModelOrEntityType), null, entityFieldMap);
        return count(specification);
    }

}

