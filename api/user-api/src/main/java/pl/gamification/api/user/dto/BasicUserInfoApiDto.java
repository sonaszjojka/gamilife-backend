package pl.gamification.api.user.dto;

import java.util.UUID;

public record BasicUserInfoApiDto(
        UUID userId,
        String email,
        String username,
        int level,
        int experience,
        int money
) {
}
