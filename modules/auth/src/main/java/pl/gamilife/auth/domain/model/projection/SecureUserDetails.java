package pl.gamilife.auth.domain.model.projection;

import java.time.Instant;
import java.util.UUID;

public record SecureUserDetails(
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
