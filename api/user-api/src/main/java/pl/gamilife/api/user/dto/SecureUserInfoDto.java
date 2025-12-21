package pl.gamilife.api.user.dto;

import java.time.Instant;
import java.util.UUID;

public record SecureUserInfoDto(
        UUID userId,
        String email,
        String username,
        String password,
        Instant passwordChangeDate,
        boolean isEmailVerified,
        boolean isTutorialCompleted,
        int money
) {
}
