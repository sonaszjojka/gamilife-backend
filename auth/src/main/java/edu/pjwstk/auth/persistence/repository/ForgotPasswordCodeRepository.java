package edu.pjwstk.auth.persistence.repository;

import edu.pjwstk.auth.domain.ForgotPasswordCode;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface ForgotPasswordCodeRepository {
    void create(ForgotPasswordCode emailVerification);

    Optional<ForgotPasswordCode> findByUserIdAndCode(UUID userId, String code);

    List<ForgotPasswordCode> findByUserIdAndNotRevokedOrderByIssuedAt(UUID userId);
}
