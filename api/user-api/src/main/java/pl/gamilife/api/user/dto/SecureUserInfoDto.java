package pl.gamilife.api.user.dto;

import java.util.UUID;

public record SecureUserInfoDto(
        UUID userId,
        String email,
        String username,
        String password,
        boolean isEmailVerified,
        boolean isTutorialCompleted,
        int money
) {
}
