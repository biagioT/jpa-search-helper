package app.tozzi.repository;

import app.tozzi.model.input.JPASearchInput;
import jakarta.persistence.criteria.JoinType;
import lombok.NonNull;

import java.util.List;
import java.util.Map;

public interface JPAProjectionRepository<E> {


    /**
     * Mode 1: Projected search by filters without sorting and pagination
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projection(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass);

    /**
     * Mode 2: Projected search by filters without sorting and pagination
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projection(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass);


    /**
     * Mode 1: Projected search by filters without sorting and pagination and with forced fetched Join
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @param fetches                 fetches joins
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projection(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<?> entityClass, Map<String, JoinType> fetches);

    /**
     * Mode 2: Projected search by filters without sorting and pagination and with forced fetched Join
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @param fetches                 fetches joins
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projection(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<?> entityClass, Map<String, JoinType> fetches);

    /**
     * Mode 1: Projected search by filters without sorting and pagination, with forced fetched Join and with a map of:
     * <ul>
     * <li> key: domain object field name </li>
     * <li> value: entity field name </li>
     * </ul>
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @param fetches                 fetches joins
     * @param entityFieldMap
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projection(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<?> entityClass, Map<String, JoinType> fetches, Map<String, String> entityFieldMap);

    /**
     * Mode 2: Projected search by filters without sorting and pagination, with forced fetched Join and with a map of:
     * <ul>
     * <li> key: domain object field name </li>
     * <li> value: entity field name </li>
     * </ul>
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @param fetches                 fetches joins
     * @param entityFieldMap
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projection(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<?> entityClass, Map<String, JoinType> fetches, Map<String, String> entityFieldMap);

    /**
     * Mode 1: Projected search by filters with sorting and without pagination
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projectionWithSorting(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass);

    /**
     * Mode 2: Projected search by filters with sorting and without pagination
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projectionWithSorting(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass);

    /**
     * Mode 1: Projected search by filters with sorting and pagination
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projectionPaginated(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass);

    /**
     * Mode 2: Projected search by filters with sorting and pagination
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projectionPaginated(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass);

    /**
     * Mode 1: Projected search by filters with sorting and pagination and with a map of
     * <ul>
     * <li> key: domain object field name </li>
     * <li> value: entity field name </li>
     * </ul>
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projectionPaginated(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, String> entityFieldMap);

    /**
     * Mode 2: Projected search by filters with sorting and pagination and with a map of
     * <ul>
     * <li> key: domain object field name </li>
     * <li> value: entity field name </li>
     * </ul>
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projectionPaginated(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, String> entityFieldMap);

    // ----

    /**
     * Mode 1: Projected search by filters without sorting and pagination
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @param overrideJoinTypes       the only relationships for which you want to change the Join type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projectionClassic(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> overrideJoinTypes);

    /**
     * Mode 2: Projected search by filters without sorting and pagination
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @param overrideJoinTypes       the only relationships for which you want to change the Join type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projectionClassic(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> overrideJoinTypes);


    /**
     * Mode 1: Projected search by filters without sorting and pagination and with forced fetched Join
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @param fetches                 fetches joins
     * @param overrideJoinTypes       the only relationships for which you want to change the Join type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projectionClassic(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<?> entityClass, Map<String, JoinType> fetches, Map<String, JoinType> overrideJoinTypes);

    /**
     * Mode 2: Projected search by filters without sorting and pagination and with forced fetched Join
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @param fetches                 fetches joins
     * @param overrideJoinTypes       the only relationships for which you want to change the Join type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projectionClassic(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<?> entityClass, Map<String, JoinType> fetches, Map<String, JoinType> overrideJoinTypes);

    /**
     * Mode 1: Projected search by filters without sorting and pagination, with forced fetched Join and with a map of:
     * <ul>
     * <li> key: domain object field name </li>
     * <li> value: entity field name </li>
     * </ul>
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @param fetches                 fetches joins
     * @param entityFieldMap
     * @param overrideJoinTypes       the only relationships for which you want to change the Join type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projectionClassic(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<?> entityClass, Map<String, JoinType> fetches, Map<String, String> entityFieldMap, Map<String, JoinType> overrideJoinTypes);

    /**
     * Mode 2: Projected search by filters without sorting and pagination, with forced fetched Join and with a map of:
     * <ul>
     * <li> key: domain object field name </li>
     * <li> value: entity field name </li>
     * </ul>
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @param fetches                 fetches joins
     * @param entityFieldMap
     * @param overrideJoinTypes       the only relationships for which you want to change the Join type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projectionClassic(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<?> entityClass, Map<String, JoinType> fetches, Map<String, String> entityFieldMap, Map<String, JoinType> overrideJoinTypes);

    /**
     * Mode 1: Projected search by filters with sorting and without pagination
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @param overrideJoinTypes       the only relationships for which you want to change the Join type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projectionWithSortingClassic(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> overrideJoinTypes);

    /**
     * Mode 2: Projected search by filters with sorting and without pagination
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @param overrideJoinTypes       the only relationships for which you want to change the Join type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projectionWithSortingClassic(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> overrideJoinTypes);

    /**
     * Mode 1: Projected search by filters with sorting and pagination
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @param overrideJoinTypes       the only relationships for which you want to change the Join type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projectionPaginatedClassic(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> overrideJoinTypes);

    /**
     * Mode 2: Projected search by filters with sorting and pagination
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @param overrideJoinTypes       the only relationships for which you want to change the Join type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projectionPaginatedClassic(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> overrideJoinTypes);

    /**
     * Mode 1: Projected search by filters with sorting and pagination and with a map of
     * <ul>
     * <li> key: domain object field name </li>
     * <li> value: entity field name </li>
     * </ul>
     *
     * @param filters                 search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @param overrideJoinTypes       the only relationships for which you want to change the Join type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projectionPaginatedClassic(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, String> entityFieldMap, Map<String, JoinType> overrideJoinTypes);

    /**
     * Mode 2: Projected search by filters with sorting and pagination and with a map of
     * <ul>
     * <li> key: domain object field name </li>
     * <li> value: entity field name </li>
     * </ul>
     *
     * @param input                   search filters
     * @param domainModelOrEntityType the type of the domain object or entity: i.e. the root object where you applied the {@link app.tozzi.annotation.Projectable} and {@link app.tozzi.annotation.NestedProjectable} annotations
     * @param entityClass             entity type
     * @param overrideJoinTypes       the only relationships for which you want to change the Join type
     * @return list of results in map format whose keys correspond to the values of the entity fields
     */
    List<Map<String, Object>> projectionPaginatedClassic(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, String> entityFieldMap, Map<String, JoinType> overrideJoinTypes);


}
