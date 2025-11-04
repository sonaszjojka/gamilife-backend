package edu.pjwstk.auth.service;

import java.util.UUID;

public interface EmailVerificationCodeService {
    String generateAndSaveEmailVerificationCode(UUID userId);

    String hashCode(String code);

    void revokeAllActiveEmailVerificationCodesByUserId(UUID userId);
}
