package edu.pjwstk.auth.dto.service;

import java.time.LocalDateTime;
import java.util.UUID;

public record RefreshToken(
        UUID id,
        UUID userId,
        String token,
        LocalDateTime issuedAt,
        LocalDateTime expiresAt,
        boolean revoked
) {
}

