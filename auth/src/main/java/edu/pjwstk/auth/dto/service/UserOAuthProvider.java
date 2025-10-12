package edu.pjwstk.auth.dto.service;

import java.util.UUID;

public record UserOAuthProvider(
        UUID id,
        UUID userId,
        String provider,
        String providerId

) {
}
