package edu.pjwstk.api.auth.dto;

import java.util.UUID;

public record RotateUserTokensCommand(
        UUID userId,
        String email,
        boolean isEmailVerified
) {
}
