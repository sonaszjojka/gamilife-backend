package pl.gamilife.auth.domain.model.projection;

import java.util.UUID;

public record BasicUserDetails(
        UUID userId,
        String email,
        String username,
        int level,
        int experience,
        int money
) {
}
