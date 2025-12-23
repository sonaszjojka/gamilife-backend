package pl.gamilife.gamification.domain.model.projection;

import java.util.UUID;

public record GamificationUser(
        UUID userId,
        String username,
        int level,
        int experience,
        int money
) {
}
