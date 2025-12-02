package edu.pjwstk.auth.repository;

import edu.pjwstk.auth.models.UserOAuthProvider;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Optional;
import java.util.UUID;

public interface JpaUserProviderRepository extends JpaRepository<UserOAuthProvider, UUID> {

    boolean existsByUserIdAndProvider(UUID userId, String provider);

    @Query("""
                SELECT up
                FROM UserOAuthProvider up
                WHERE up.provider = :providerName
                AND up.providerId = :providerId
            """)
    Optional<UserOAuthProvider> findByProviderAndProviderId(
            @Param("providerName") String providerName,
            @Param("providerId") String providerId
    );
}
