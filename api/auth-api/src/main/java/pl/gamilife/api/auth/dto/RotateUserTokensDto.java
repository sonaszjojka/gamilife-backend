package pl.gamilife.api.auth.dto;

import java.util.UUID;

public record RotateUserTokensDto(
        UUID userId,
        String email,
        boolean isEmailVerified
) {
}
