package edu.pjwstk.auth.service;

import java.util.UUID;

public interface ForgotPasswordCodeService {
    String generateAndSaveForgotPasswordCode();

    String hashCode(String code);

    void revokeAllActiveForgotPasswordCodesByUserId(UUID userId);
}
