package com.gisgro.repository;

import com.gisgro.JPASearchCore;
import javax.persistence.criteria.JoinType;

import com.fasterxml.jackson.databind.JsonNode;
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
    default List<E> findAll(@NonNull JsonNode filters, @NonNull Class<?> type) {
        Specification<E> specification = JPASearchCore.specification(filters, type, true);
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
    default List<E> findAll(@NonNull JsonNode filters, @NonNull Class<?> type, Map<String, JoinType> fetches) {
        Specification<E> specification = JPASearchCore.specification(filters, type, fetches, true);
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
    default List<E> findAll(@NonNull JsonNode filters, @NonNull Class<?> type, Map<String, JoinType> fetches, Map<String, String> entityFieldMap) {
        Specification<E> specification = JPASearchCore.specification(filters, type, fetches, true, entityFieldMap);
        return findAll(specification);
    }

    /**
     * Search by filters.
     *
     * @param filters
     * @param type
     * @return
     */
    default List<E> findAllSorted(@NonNull JsonNode filters, @NonNull Class<?> type) {
        Specification<E> specification = JPASearchCore.specification(filters, type, true);
        Sort sort = JPASearchCore.loadSort(filters, type, true, true, null);
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
    default List<E> findAllSorted(@NonNull JsonNode filters, @NonNull Class<?> type, Map<String, JoinType> fetches) {
        Specification<E> specification = JPASearchCore.specification(filters, type, fetches, true);
        Sort sort = JPASearchCore.loadSort(filters, type, true, true, null);
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
    default List<E> findAllSorted(@NonNull JsonNode filters, @NonNull Class<?> type, Map<String, JoinType> fetches, Map<String, String> entityFieldMap) {
        Specification<E> specification = JPASearchCore.specification(filters, type, fetches, true, entityFieldMap);
        Sort sort = JPASearchCore.loadSort(filters, type, true, true, entityFieldMap);
        return findAll(specification, sort);
    }

    /**
     * Paginated search
     *
     * @param filters
     * @param type
     * @return
     */
    default Page<E> findAllWithPaginationAndSorting(@NonNull JsonNode filters, @NonNull Class<?> type) {
        Specification<E> specification = JPASearchCore.specification(filters, type, true);
        PageRequest pageRequest = JPASearchCore.loadSortAndPagination(filters, type, true, true, null);
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
    default Page<E> findAllWithPaginationAndSorting(@NonNull JsonNode filters, @NonNull Class<?> type, Map<String, String> entityFieldMap) {
        Specification<E> specification = JPASearchCore.specification(filters, type, true, entityFieldMap);
        PageRequest pageRequest = JPASearchCore.loadSortAndPagination(filters, type, true, true, entityFieldMap);
        return findAll(specification, pageRequest);
    }

    /**
     * Count
     *
     * @param filters
     * @param type
     * @return
     */
    default long count(@NonNull JsonNode filters, @NonNull Class<?> type) {
        Specification<E> specification = JPASearchCore.specification(filters, type, true);
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
    default long count(@NonNull JsonNode filters, @NonNull Class<?> type, Map<String, String> entityFieldMap) {
        Specification<E> specification = JPASearchCore.specification(filters, type, true, entityFieldMap);
        return count(specification);
    }

}

