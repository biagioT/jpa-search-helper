package app.tozzi;

import app.tozzi.repository.JPAProjectionRepository;
import app.tozzi.repository.JPAProjectionRepositoryImpl;
import jakarta.persistence.EntityManager;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;

/**
 * Spring Boot auto-configuration for the JPA Search Helper library.
 * <p>
 * When a consuming application has {@code spring-boot-autoconfigure} on its classpath (i.e. it is a
 * Spring Boot app), this auto-configuration registers the {@link JPAProjectionRepositoryImpl} bean
 * required by {@link JPAProjectionRepository} fragments, so the consumer does not need to add
 * {@code app.tozzi} to {@code @ComponentScan} or {@code @EnableJpaRepositories} explicitly.
 * <p>
 * The bean is registered only if no other bean of the same type is already present in the application
 * context: this preserves backward compatibility with consumers that already declare an explicit
 * component scan over the {@code app.tozzi} package.
 */
@AutoConfiguration
@ConditionalOnClass(EntityManager.class)
public class JPASearchAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean
    public JPAProjectionRepositoryImpl<?> jpaProjectionRepositoryImpl() {
        return new JPAProjectionRepositoryImpl<>();
    }
}

