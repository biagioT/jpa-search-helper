package app.tozzi.repository;

import app.tozzi.core.JPAProjectionProcessor;
import app.tozzi.model.input.JPASearchInput;
import app.tozzi.util.JPASearchUtils;
import app.tozzi.util.ReflectionUtils;
import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.JoinType;
import lombok.NonNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

@Component
public class JPAProjectionRepositoryImpl<E> implements JPAProjectionRepository<E> {

    @Autowired
    private EntityManager entityManager;

    @Override
    public List<Map<String, Object>> projection(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass) {
        return performProjection(filters, domainModelOrEntityType, entityClass, null, null, false, false, null);
    }

    @Override
    public List<Map<String, Object>> projection(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass) {
        return performProjection(input, domainModelOrEntityType, entityClass, null, null, false, false, null);
    }

    @Override
    public List<Map<String, Object>> projection(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> fetches) {
        return performProjection(filters, domainModelOrEntityType, entityClass, fetches, null, false, false, null);
    }

    @Override
    public List<Map<String, Object>> projection(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> fetches) {
        return performProjection(input, domainModelOrEntityType, entityClass, fetches, null, false, false, null);
    }

    @Override
    public List<Map<String, Object>> projection(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> fetches, Map<String, String> entityFieldMap) {
        return performProjection(filters, domainModelOrEntityType, entityClass, fetches, entityFieldMap, false, false, null);
    }

    @Override
    public List<Map<String, Object>> projection(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> fetches, Map<String, String> entityFieldMap) {
        return performProjection(input, domainModelOrEntityType, entityClass, fetches, entityFieldMap, false, false, null);
    }

    @Override
    public List<Map<String, Object>> projectionWithSorting(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass) {
        return performProjection(filters, domainModelOrEntityType, entityClass, null, null, true, false, null);
    }

    @Override
    public List<Map<String, Object>> projectionWithSorting(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass) {
        return performProjection(input, domainModelOrEntityType, entityClass, null, null, true, false, null);
    }

    @Override
    public List<Map<String, Object>> projectionWithSorting(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, String> entityFieldMap) {
        return performProjection(filters, domainModelOrEntityType, entityClass, null, entityFieldMap, true, false, null);
    }

    @Override
    public List<Map<String, Object>> projectionWithSorting(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, String> entityFieldMap) {
        return performProjection(input, domainModelOrEntityType, entityClass, null, entityFieldMap, true, false, null);
    }

    @Override
    public List<Map<String, Object>> projectionWithSorting(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> fetches, Map<String, String> entityFieldMap) {
        return performProjection(filters, domainModelOrEntityType, entityClass, fetches, entityFieldMap, true, false, null);
    }

    @Override
    public List<Map<String, Object>> projectionWithSorting(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> fetches, Map<String, String> entityFieldMap) {
        return performProjection(input, domainModelOrEntityType, entityClass, fetches, entityFieldMap, true, false, null);
    }

    // ---

    @Override
    public List<Map<String, Object>> projectionClassic(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> overrideJoinTypes) {
        return performProjection(filters, domainModelOrEntityType, entityClass, null, null, false, true, overrideJoinTypes);
    }

    @Override
    public List<Map<String, Object>> projectionClassic(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> overrideJoinTypes) {
        return performProjection(input, domainModelOrEntityType, entityClass, null, null, false, true, overrideJoinTypes);
    }

    @Override
    public List<Map<String, Object>> projectionClassic(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> fetches, Map<String, JoinType> overrideJoinTypes) {
        return performProjection(filters, domainModelOrEntityType, entityClass, fetches, null, false, true, overrideJoinTypes);
    }

    @Override
    public List<Map<String, Object>> projectionClassic(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> fetches, Map<String, JoinType> overrideJoinTypes) {
        return performProjection(input, domainModelOrEntityType, entityClass, fetches, null, false, true, overrideJoinTypes);
    }

    @Override
    public List<Map<String, Object>> projectionClassic(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> fetches, Map<String, String> entityFieldMap, Map<String, JoinType> overrideJoinTypes) {
        return performProjection(filters, domainModelOrEntityType, entityClass, fetches, entityFieldMap, false, true, overrideJoinTypes);
    }

    @Override
    public List<Map<String, Object>> projectionClassic(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> fetches, Map<String, String> entityFieldMap, Map<String, JoinType> overrideJoinTypes) {
        return performProjection(input, domainModelOrEntityType, entityClass, fetches, entityFieldMap, false, true, overrideJoinTypes);
    }

    @Override
    public List<Map<String, Object>> projectionWithSortingClassic(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> overrideJoinTypes) {
        return performProjection(filters, domainModelOrEntityType, entityClass, null, null, true, true, overrideJoinTypes);
    }

    @Override
    public List<Map<String, Object>> projectionWithSortingClassic(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> overrideJoinTypes) {
        return performProjection(input, domainModelOrEntityType, entityClass, null, null, true, true, overrideJoinTypes);
    }

    @Override
    public List<Map<String, Object>> projectionWithSortingClassic(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> fetches, Map<String, JoinType> overrideJoinTypes) {
        return performProjection(filters, domainModelOrEntityType, entityClass, fetches, null, true, true, overrideJoinTypes);
    }

    @Override
    public List<Map<String, Object>> projectionWithSortingClassic(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> fetches, Map<String, JoinType> overrideJoinTypes) {
        return performProjection(input, domainModelOrEntityType, entityClass, fetches, null, true, true, overrideJoinTypes);
    }

    @Override
    public List<Map<String, Object>> projectionWithSortingClassic(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> fetches, Map<String, String> entityFieldMap, Map<String, JoinType> overrideJoinTypes) {
        return performProjection(filters, domainModelOrEntityType, entityClass, fetches, entityFieldMap, true, true, overrideJoinTypes);
    }

    @Override
    public List<Map<String, Object>> projectionWithSortingClassic(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> fetches, Map<String, String> entityFieldMap, Map<String, JoinType> overrideJoinTypes) {
        return performProjection(input, domainModelOrEntityType, entityClass, fetches, entityFieldMap, true, true, overrideJoinTypes);
    }

    private List<Map<String, Object>> performProjection(
            Map<String, String> inputMap,
            @NonNull Class<?> domainModelOrEntityType,
            @NonNull Class<E> entityClass,
            Map<String, JoinType> fetches,
            Map<String, String> entityFieldMap,
            boolean withSorting,
            boolean overrideJoinTypes,
            Map<String, JoinType> overrideJoinTypesMap) {

        var input = JPASearchUtils.toObject(inputMap, false, withSorting, true);
        return performProjection(input, domainModelOrEntityType, entityClass, fetches, entityFieldMap, withSorting, overrideJoinTypes, overrideJoinTypesMap);
    }

    private List<Map<String, Object>> performProjection(
            JPASearchInput input,
            @NonNull Class<?> domainModelOrEntityType,
            @NonNull Class<E> entityClass,
            Map<String, JoinType> fetches,
            Map<String, String> entityFieldMap,
            boolean withSorting,
            boolean overrideJoinTypes,
            Map<String, JoinType> overrideJoinTypesMap) {

        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var idFields = ReflectionUtils.getIdFields(entityClass);

        var query = JPAProjectionProcessor.getQuery(
                input, domainModelOrEntityType, entityClass,
                entityManager.getCriteriaBuilder(), idFields,
                withSorting, fetches, entityFieldMap, searchableFields,
                overrideJoinTypes, overrideJoinTypesMap);

        var typedQuery = entityManager.createQuery(query.getCriteriaQuery());
        return JPAProjectionProcessor.toMap(typedQuery.getResultList(), entityClass, query.getSelections(), idFields);
    }

}
