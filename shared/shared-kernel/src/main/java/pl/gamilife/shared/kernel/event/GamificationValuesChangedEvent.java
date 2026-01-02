package pl.gamilife.shared.kernel.event;

import java.util.UUID;

public record GamificationValuesChangedEvent(
        UUID userId,
        String username,
        int level,
        int experience,
        int money,
        Integer requiredExperienceForNextLevel
) {
}
