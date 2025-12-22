package pl.gamilife.auth.domain.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.validation.constraints.NotNull;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.time.Instant;
import java.util.UUID;

@Getter
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "email_verification_code")
public class EmailVerificationCode extends BaseEntity {

    @NotNull
    @Column(name = "user_id", nullable = false)
    private UUID userId;

    @NotNull
    @Column(name = "code", nullable = false)
    private String code;

    @Column(name = "issued_at", nullable = false)
    private final Instant issuedAt = Instant.now();

    @Column(name = "expires_at", nullable = false)
    private Instant expiresAt;

    @Column(name = "revoked", nullable = false)
    private boolean revoked = false;

    private EmailVerificationCode(UUID userId, String code, Instant expiresAt) {
        setUserId(userId);
        setCode(code);
        setExpiresAt(expiresAt);
    }

    public static EmailVerificationCode create(UUID userId, String code, long emailVerificationTimeout) {
        return new EmailVerificationCode(
                userId,
                code,
                Instant.now().plusSeconds(emailVerificationTimeout)
        );
    }

    private void setUserId(UUID userId) {
        if (userId == null) {
            throw new IllegalArgumentException("User ID cannot be null"); // TODO: change exc type after task refactor merge
        }

        this.userId = userId;
    }

    private void setCode(String code) {
        if (code == null || code.isBlank()) {
            throw new IllegalArgumentException("Code cannot be null"); // TODO: change exc type after task refactor merge
        }

        this.code = code;
    }

    private void setExpiresAt(Instant expiresAt) {
        if (expiresAt == null) {
            throw new IllegalArgumentException("Expires at cannot be null"); // TODO: change exc type after task refactor merge
        }

        if (expiresAt.isBefore(issuedAt)) {
            throw new IllegalArgumentException("Expires at cannot be before issued at"); // TODO: change exc type after task refactor merge
        }

        this.expiresAt = expiresAt;
    }
}
