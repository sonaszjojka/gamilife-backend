package pl.gamilife.auth.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Repository;
import pl.gamilife.auth.domain.model.UserOAuthProvider;
import pl.gamilife.auth.domain.port.repository.UserProviderRepository;
import pl.gamilife.auth.infrastructure.persistence.jpa.JpaUserProviderRepository;

import java.util.Optional;
import java.util.UUID;

@Repository
@AllArgsConstructor
public class UserProviderRepositoryImpl implements UserProviderRepository {

    private final JpaUserProviderRepository jpaUserProviderRepository;

    @Override
    public void save(UserOAuthProvider userOAuthProvider) {
        jpaUserProviderRepository.save(userOAuthProvider);
    }

    @Override
    public Optional<UserOAuthProvider> findByProviderAndProviderId(String providerName, String providerId) {
        return jpaUserProviderRepository.findByProviderAndProviderId(providerName, providerId);
    }

    @Override
    public boolean existsByUserIdAndProvider(UUID userId, String providerName) {
        return jpaUserProviderRepository.existsByUserIdAndProvider(userId, providerName);
    }
}
