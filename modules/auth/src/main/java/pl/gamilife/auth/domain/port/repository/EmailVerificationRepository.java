package pl.gamilife.auth.domain.port.repository;

import pl.gamilife.auth.domain.model.EmailVerificationCode;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface EmailVerificationRepository {
    void save(EmailVerificationCode emailVerification);

    void revokeAllActiveEmailVerificationCodesByUserId(UUID userId);

    Optional<EmailVerificationCode> findByUserIdAndCode(UUID userId, String hashedCode);

    List<EmailVerificationCode> findByUserIdAndRevokedOrderByIssuedAtDesc(UUID userId, boolean revoked);
}
