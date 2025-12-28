package pl.gamilife.auth.domain.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.util.UUID;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Entity
@Table(name = "user_oauth_provider", schema = "security")
public class UserOAuthProvider extends BaseEntity {

    @Column(name = "user_id", nullable = false)
    private UUID userId;

    @Column(name = "provider", nullable = false)
    private String provider;

    @Column(name = "provider_id", nullable = false)
    private String providerId;

    private UserOAuthProvider(UUID userId, String provider, String providerId) {
        setUserId(userId);
        setProvider(provider);
        setProviderId(providerId);
    }

    public static UserOAuthProvider create(UUID userId, String provider, String providerId) {
        return new UserOAuthProvider(userId, provider, providerId);
    }

    public void setUserId(UUID userId) {
        if (userId == null) {
            throw new DomainValidationException("User ID cannot be null");
        }

        this.userId = userId;
    }

    public void setProvider(String provider) {
        if (provider == null || provider.isBlank()) {
            throw new DomainValidationException("Provider cannot be null");
        }

        this.provider = provider;
    }

    public void setProviderId(String providerId) {
        if (providerId == null || providerId.isBlank()) {
            throw new DomainValidationException("Provider ID cannot be null");
        }

        this.providerId = providerId;
    }
}
