package pl.gamilife.shared.kernel.event;

import java.util.UUID;

public record AchievementUnlockedEvent(
        UUID userId,
        String achievementName
) {
}
