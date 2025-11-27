package edu.pjwstk.api.auth.dto;

import java.util.UUID;

public record RotateUserTokensDto(
        UUID userId,
        String email,
        boolean isEmailVerified
) {
}
