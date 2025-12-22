package pl.gamilife.auth.application.dto;

import java.util.UUID;

public record TokenClaims(
        UUID userId,
        String email,
        boolean isEmailVerified
) {
}
