package edu.pjwstk.gamification.repository.query;

import java.util.UUID;

public record AchievementDetailsDto(
        UUID id,
        String name,
        String description,
        String imagePath,
        Integer goal,
        Boolean isUnlocked,
        Integer progress
) {
}
