package pl.gamilife.gamification.application.usecase.getgamificationuser;

import java.util.UUID;

public record GetGamificationUserResult(
        UUID userId,
        String username,
        int level,
        int experience,
        int money,
        Integer requiredExperienceForNextLevel
) {
}
