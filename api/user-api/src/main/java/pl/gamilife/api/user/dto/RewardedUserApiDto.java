package pl.gamilife.api.user.dto;

import java.util.UUID;

public record RewardedUserApiDto(
        UUID userId,
        String username,
        int experience,
        int money,
        int level
) {
}
