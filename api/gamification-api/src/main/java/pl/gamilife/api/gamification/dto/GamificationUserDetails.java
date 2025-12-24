package pl.gamilife.api.gamification.dto;

import java.util.UUID;

public record GamificationUserDetails(
        UUID userId,
        Integer requiredExperienceForNextLevel
) {
}
