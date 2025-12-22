package pl.gamilife.auth.domain.model.projection;

import java.util.UUID;

public record SecureUserDetails(
        UUID userId,
        String email,
        String username,
        String password,
        boolean isEmailVerified,
        boolean isTutorialCompleted,
        int money
) {
}
