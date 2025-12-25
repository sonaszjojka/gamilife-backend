package pl.gamilife.auth.domain.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.time.Instant;
import java.util.UUID;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Entity
@Table(name = "refresh_token")
public class RefreshToken extends BaseEntity {

    @Column(name = "user_id", nullable = false)
    private UUID userId;

    @Column(name = "token", nullable = false, unique = true)
    private String token;

    @Column(name = "issued_at", nullable = false)
    private final Instant issuedAt = Instant.now();

    @Column(name = "expires_at", nullable = false)
    private Instant expiresAt;

    @Column(name = "revoked", nullable = false)
    private boolean revoked = false;

    private RefreshToken(UUID userId, String code, Instant expiresAt) {
        setUserId(userId);
        setToken(code);
        setExpiresAt(expiresAt);
    }

    public static RefreshToken create(UUID userId, String code, long refreshTokenTimeout) {
        return new RefreshToken(
                userId,
                code,
                Instant.now().plusSeconds(refreshTokenTimeout)
        );
    }

    private void setUserId(UUID userId) {
        if (userId == null) {
            throw new DomainValidationException("User ID cannot be null");
        }

        this.userId = userId;
    }

    private void setToken(String token) {
        if (token == null || token.isBlank()) {
            throw new DomainValidationException("Code cannot be null");
        }

        this.token = token;
    }

    private void setExpiresAt(Instant expiresAt) {
        if (expiresAt == null) {
            throw new DomainValidationException("Expires at cannot be null");
        }

        if (expiresAt.isBefore(issuedAt)) {
            throw new DomainValidationException("Expires at cannot be before issued at");
        }

        this.expiresAt = expiresAt;
    }
}
