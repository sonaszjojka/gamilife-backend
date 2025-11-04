package edu.pjwstk.auth.persistence.repository;

import edu.pjwstk.auth.domain.EmailVerification;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface EmailVerificationRepository {
    void create(EmailVerification emailVerification);

    Optional<EmailVerification> findByUserIdAndCode(UUID userId, String code);

    List<EmailVerification> findByUserIdAndNotRevokedOrderByIssuedAt(UUID userId);

    void revokeAllActiveEmailVerificationCodesByUserId(UUID userId);
}
