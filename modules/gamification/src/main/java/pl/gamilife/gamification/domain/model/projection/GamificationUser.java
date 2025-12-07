package pl.gamilife.gamification.domain.model.projection;

import java.util.UUID;

public record GamificationUser(
        UUID userId,
        int level,
        int experience,
        int money
) {
}
