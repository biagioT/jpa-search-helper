package app.tozzi.repository;

import app.tozzi.core.JPAProjectionProcessor;
import app.tozzi.model.JPAProjectionOptions;
import app.tozzi.model.input.JPASearchInput;
import app.tozzi.util.JPASearchUtils;
import app.tozzi.util.ReflectionUtils;
import jakarta.persistence.EntityManager;
import lombok.NonNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

@Component
public class JPAProjectionRepositoryImpl<E> implements JPAProjectionRepository<E> {

    @Autowired
    private EntityManager entityManager;

    // ---------- projection ----------

    @Override
    public List<Map<String, Object>> projection(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass) {
        return projection(input, domainModelOrEntityType, entityClass, JPAProjectionOptions.empty());
    }

    @Override
    public List<Map<String, Object>> projection(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass) {
        return projection(filters, domainModelOrEntityType, entityClass, JPAProjectionOptions.empty());
    }

    @Override
    public List<Map<String, Object>> projection(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, @NonNull JPAProjectionOptions options) {
        return performProjection(input, domainModelOrEntityType, entityClass, options, false);
    }

    @Override
    public List<Map<String, Object>> projection(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, @NonNull JPAProjectionOptions options) {
        return performProjection(JPASearchUtils.toObject(filters, false, false, true), domainModelOrEntityType, entityClass, options, false);
    }

    // ---------- projectionWithSorting ----------

    @Override
    public List<Map<String, Object>> projectionWithSorting(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass) {
        return projectionWithSorting(input, domainModelOrEntityType, entityClass, JPAProjectionOptions.empty());
    }

    @Override
    public List<Map<String, Object>> projectionWithSorting(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass) {
        return projectionWithSorting(filters, domainModelOrEntityType, entityClass, JPAProjectionOptions.empty());
    }

    @Override
    public List<Map<String, Object>> projectionWithSorting(@NonNull JPASearchInput input, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, @NonNull JPAProjectionOptions options) {
        return performProjection(input, domainModelOrEntityType, entityClass, options, true);
    }

    @Override
    public List<Map<String, Object>> projectionWithSorting(@NonNull Map<String, String> filters, @NonNull Class<?> domainModelOrEntityType, @NonNull Class<E> entityClass, @NonNull JPAProjectionOptions options) {
        return performProjection(JPASearchUtils.toObject(filters, false, true, true), domainModelOrEntityType, entityClass, options, true);
    }

    // ---------- internals ----------

    private List<Map<String, Object>> performProjection(
            JPASearchInput input,
            @NonNull Class<?> domainModelOrEntityType,
            @NonNull Class<E> entityClass,
            @NonNull JPAProjectionOptions options,
            boolean withSorting) {

        var searchableFields = ReflectionUtils.getAllSearchableFields(domainModelOrEntityType);
        var idFields = ReflectionUtils.getIdFields(entityClass);

        var query = JPAProjectionProcessor.getQuery(
                input, domainModelOrEntityType, entityClass,
                entityManager.getCriteriaBuilder(), idFields,
                withSorting, options.getFetches(), options.getEntityFieldMap(), searchableFields,
                options.isUseOverrideJoinTypes(), options.getOverrideJoinTypes());

        var typedQuery = entityManager.createQuery(query.getCriteriaQuery());
        return JPAProjectionProcessor.toMap(typedQuery.getResultList(), entityClass, query.getSelections(), idFields);
    }
}

