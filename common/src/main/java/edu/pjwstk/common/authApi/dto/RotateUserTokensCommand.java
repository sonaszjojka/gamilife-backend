package edu.pjwstk.common.authApi.dto;

import java.util.UUID;

public record RotateUserTokensCommand(
        UUID userId,
        String email,
        boolean isEmailVerified
) {
}
