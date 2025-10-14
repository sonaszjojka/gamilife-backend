package edu.pjwstk.auth.domain;

import java.util.UUID;

public record UserOAuthProvider(
        UUID id,
        UUID userId,
        String provider,
        String providerId

) {
}
