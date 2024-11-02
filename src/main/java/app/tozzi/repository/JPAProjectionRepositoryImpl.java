package app.tozzi.repository;

import app.tozzi.core.JPAProjectionProcessor;
import app.tozzi.model.input.JPASearchInput;
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
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var idFields = ReflectionUtils.getIdFields(entityClass);
        var query = JPAProjectionProcessor.getQuery(filters, domainModelOrEntityType, entityClass, entityManager.getCriteriaBuilder(), idFields, false, null, null, searchableFields);
        var typedQuery = entityManager.createQuery(query.getCriteriaQuery());
        return JPAProjectionProcessor.toMap(typedQuery.getResultList(), entityClass, query.getSelections(), idFields);
    }

    @Override
    public List<Map<String, Object>> projection(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var idFields = ReflectionUtils.getIdFields(entityClass);
        var query = JPAProjectionProcessor.getQuery(input, domainModelOrEntityType, entityClass, entityManager.getCriteriaBuilder(), idFields, false, null, null, searchableFields);
        var typedQuery = entityManager.createQuery(query.getCriteriaQuery());
        return JPAProjectionProcessor.toMap(typedQuery.getResultList(), entityClass, query.getSelections(), idFields);
    }

    @Override
    public List<Map<String, Object>> projection(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<?> entityClass, Map<String, JoinType> fetches) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var idFields = ReflectionUtils.getIdFields(entityClass);
        var query = JPAProjectionProcessor.getQuery(filters, domainModelOrEntityType, entityClass, entityManager.getCriteriaBuilder(), idFields, false, fetches, null, searchableFields);
        var typedQuery = entityManager.createQuery(query.getCriteriaQuery());
        return JPAProjectionProcessor.toMap(typedQuery.getResultList(), entityClass, query.getSelections(), idFields);
    }

    @Override
    public List<Map<String, Object>> projection(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<?> entityClass, Map<String, JoinType> fetches) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var idFields = ReflectionUtils.getIdFields(entityClass);
        var query = JPAProjectionProcessor.getQuery(input, domainModelOrEntityType, entityClass, entityManager.getCriteriaBuilder(), idFields, false, fetches, null, searchableFields);
        var typedQuery = entityManager.createQuery(query.getCriteriaQuery());
        return JPAProjectionProcessor.toMap(typedQuery.getResultList(), entityClass, query.getSelections(), idFields);
    }

    @Override
    public List<Map<String, Object>> projection(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<?> entityClass, Map<String, JoinType> fetches, Map<String, String> entityFieldMap) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var idFields = ReflectionUtils.getIdFields(entityClass);
        var query = JPAProjectionProcessor.getQuery(filters, domainModelOrEntityType, entityClass, entityManager.getCriteriaBuilder(), idFields, false, fetches, entityFieldMap, searchableFields);
        var typedQuery = entityManager.createQuery(query.getCriteriaQuery());
        return JPAProjectionProcessor.toMap(typedQuery.getResultList(), entityClass, query.getSelections(), idFields);
    }

    @Override
    public List<Map<String, Object>> projection(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<?> entityClass, Map<String, JoinType> fetches, Map<String, String> entityFieldMap) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var idFields = ReflectionUtils.getIdFields(entityClass);
        var query = JPAProjectionProcessor.getQuery(input, domainModelOrEntityType, entityClass, entityManager.getCriteriaBuilder(), idFields, false, fetches, entityFieldMap, searchableFields);
        var typedQuery = entityManager.createQuery(query.getCriteriaQuery());
        return JPAProjectionProcessor.toMap(typedQuery.getResultList(), entityClass, query.getSelections(), idFields);
    }

    @Override
    public List<Map<String, Object>> projectionWithSorting(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var idFields = ReflectionUtils.getIdFields(entityClass);
        var query = JPAProjectionProcessor.getQuery(filters, domainModelOrEntityType, entityClass, entityManager.getCriteriaBuilder(), idFields, true, null, null, searchableFields);
        var typedQuery = entityManager.createQuery(query.getCriteriaQuery());
        return JPAProjectionProcessor.toMap(typedQuery.getResultList(), entityClass, query.getSelections(), idFields);
    }

    @Override
    public List<Map<String, Object>> projectionWithSorting(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var idFields = ReflectionUtils.getIdFields(entityClass);
        var query = JPAProjectionProcessor.getQuery(input, domainModelOrEntityType, entityClass, entityManager.getCriteriaBuilder(), idFields, true, null, null, searchableFields);
        var typedQuery = entityManager.createQuery(query.getCriteriaQuery());
        return JPAProjectionProcessor.toMap(typedQuery.getResultList(), entityClass, query.getSelections(), idFields);
    }

    // ---

    @Override
    public List<Map<String, Object>> projectionClassic(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> overrideJoinTypes) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var idFields = ReflectionUtils.getIdFields(entityClass);
        var query = JPAProjectionProcessor.getQuery(filters, domainModelOrEntityType, entityClass, entityManager.getCriteriaBuilder(), idFields, false, null, null, searchableFields, true, overrideJoinTypes);
        var typedQuery = entityManager.createQuery(query.getCriteriaQuery());
        return JPAProjectionProcessor.toMap(typedQuery.getResultList(), entityClass, query.getSelections(), idFields);
    }

    @Override
    public List<Map<String, Object>> projectionClassic(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> overrideJoinTypes) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var idFields = ReflectionUtils.getIdFields(entityClass);
        var query = JPAProjectionProcessor.getQuery(input, domainModelOrEntityType, entityClass, entityManager.getCriteriaBuilder(), idFields, false, null, null, searchableFields, true, overrideJoinTypes);
        var typedQuery = entityManager.createQuery(query.getCriteriaQuery());
        return JPAProjectionProcessor.toMap(typedQuery.getResultList(), entityClass, query.getSelections(), idFields);
    }

    @Override
    public List<Map<String, Object>> projectionClassic(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<?> entityClass, Map<String, JoinType> fetches, Map<String, JoinType> overrideJoinTypes) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var idFields = ReflectionUtils.getIdFields(entityClass);
        var query = JPAProjectionProcessor.getQuery(filters, domainModelOrEntityType, entityClass, entityManager.getCriteriaBuilder(), idFields, false, fetches, null, searchableFields, true, overrideJoinTypes);
        var typedQuery = entityManager.createQuery(query.getCriteriaQuery());
        return JPAProjectionProcessor.toMap(typedQuery.getResultList(), entityClass, query.getSelections(), idFields);
    }

    @Override
    public List<Map<String, Object>> projectionClassic(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<?> entityClass, Map<String, JoinType> fetches, Map<String, JoinType> overrideJoinTypes) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var idFields = ReflectionUtils.getIdFields(entityClass);
        var query = JPAProjectionProcessor.getQuery(input, domainModelOrEntityType, entityClass, entityManager.getCriteriaBuilder(), idFields, false, fetches, null, searchableFields, true, overrideJoinTypes);
        var typedQuery = entityManager.createQuery(query.getCriteriaQuery());
        return JPAProjectionProcessor.toMap(typedQuery.getResultList(), entityClass, query.getSelections(), idFields);
    }

    @Override
    public List<Map<String, Object>> projectionClassic(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<?> entityClass, Map<String, JoinType> fetches, Map<String, String> entityFieldMap, Map<String, JoinType> overrideJoinTypes) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var idFields = ReflectionUtils.getIdFields(entityClass);
        var query = JPAProjectionProcessor.getQuery(filters, domainModelOrEntityType, entityClass, entityManager.getCriteriaBuilder(), idFields, false, fetches, entityFieldMap, searchableFields, true, overrideJoinTypes);
        var typedQuery = entityManager.createQuery(query.getCriteriaQuery());
        return JPAProjectionProcessor.toMap(typedQuery.getResultList(), entityClass, query.getSelections(), idFields);
    }

    @Override
    public List<Map<String, Object>> projectionClassic(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<?> entityClass, Map<String, JoinType> fetches, Map<String, String> entityFieldMap, Map<String, JoinType> overrideJoinTypes) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var idFields = ReflectionUtils.getIdFields(entityClass);
        var query = JPAProjectionProcessor.getQuery(input, domainModelOrEntityType, entityClass, entityManager.getCriteriaBuilder(), idFields, false, fetches, entityFieldMap, searchableFields, true, overrideJoinTypes);
        var typedQuery = entityManager.createQuery(query.getCriteriaQuery());
        return JPAProjectionProcessor.toMap(typedQuery.getResultList(), entityClass, query.getSelections(), idFields);
    }

    @Override
    public List<Map<String, Object>> projectionWithSortingClassic(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> overrideJoinTypes) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var idFields = ReflectionUtils.getIdFields(entityClass);
        var query = JPAProjectionProcessor.getQuery(filters, domainModelOrEntityType, entityClass, entityManager.getCriteriaBuilder(), idFields, true, null, null, searchableFields, true, overrideJoinTypes);
        var typedQuery = entityManager.createQuery(query.getCriteriaQuery());
        return JPAProjectionProcessor.toMap(typedQuery.getResultList(), entityClass, query.getSelections(), idFields);
    }

    @Override
    public List<Map<String, Object>> projectionWithSortingClassic(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, Map<String, JoinType> overrideJoinTypes) {
        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var idFields = ReflectionUtils.getIdFields(entityClass);
        var query = JPAProjectionProcessor.getQuery(input, domainModelOrEntityType, entityClass, entityManager.getCriteriaBuilder(), idFields, true, null, null, searchableFields, true, overrideJoinTypes);
        var typedQuery = entityManager.createQuery(query.getCriteriaQuery());
        return JPAProjectionProcessor.toMap(typedQuery.getResultList(), entityClass, query.getSelections(), idFields);
    }

}
