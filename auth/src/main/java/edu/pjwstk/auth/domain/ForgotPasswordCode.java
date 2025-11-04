package edu.pjwstk.auth.domain;

import java.time.LocalDateTime;
import java.util.UUID;

public record ForgotPasswordCode(
        UUID id,
        UUID userId,
        String code,
        LocalDateTime issuedAt,
        LocalDateTime expiresAt,
        boolean revoked) {
}