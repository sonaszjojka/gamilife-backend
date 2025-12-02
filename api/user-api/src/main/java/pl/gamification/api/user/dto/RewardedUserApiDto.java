package pl.gamification.api.user.dto;

import java.util.UUID;

public record RewardedUserApiDto(
        UUID userId,
        int experience,
        int money,
        int level
) {
}
