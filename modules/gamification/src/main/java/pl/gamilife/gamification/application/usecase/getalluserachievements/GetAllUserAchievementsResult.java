package pl.gamilife.gamification.application.usecase.getalluserachievements;

import java.util.List;
import java.util.UUID;

public record GetAllUserAchievementsResult(List<AchievementDto> achievements) {
    public record AchievementDto(
            UUID id,
            String name,
            String description,
            String iconUrl,
            boolean isUnlocked,
            int progress,
            int goal
    ) {
    }
}
