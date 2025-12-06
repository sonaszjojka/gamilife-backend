package pl.gamilife.gamification.domain.model.projection;

import java.util.UUID;

public record UserAchievementDetails(
        UUID id,
        String name,
        String description,
        String imagePath,
        Integer goal,
        Boolean isUnlocked,
        Integer progress
) {
}
