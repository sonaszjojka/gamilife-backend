package pl.gamilife.auth.domain.port.repository;

import pl.gamilife.auth.domain.model.UserOAuthProvider;

import java.util.Optional;
import java.util.UUID;

public interface UserProviderRepository {
    void save(UserOAuthProvider userOAuthProvider);

    Optional<UserOAuthProvider> findByProviderAndProviderId(String providerName, String providerId);

    boolean existsByUserIdAndProvider(UUID userId, String providerName);
}
