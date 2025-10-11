package edu.pjwstk.auth.persistence.repository.jpa;

import edu.pjwstk.auth.persistence.entity.UserOAuthProviderEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Optional;
import java.util.UUID;

public interface JpaUserProviderRepository extends JpaRepository<UserOAuthProviderEntity, UUID> {

    boolean existsByUserIdAndProvider(UUID userId, String provider);

    @Query("""
        SELECT up
        FROM UserOAuthProviderEntity up
        WHERE up.provider = :providerName
        AND up.providerId = :providerId
    """)
    Optional<UserOAuthProviderEntity> findByProviderAndProviderId(
            @Param("providerName") String providerName,
            @Param("providerId") String providerId
    );
}
