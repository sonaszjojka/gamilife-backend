package pl.gamilife.api.user.dto;

import java.util.UUID;

public record BasicUserInfoDto(
        UUID userId,
        String email,
        String username,
        int level,
        int experience,
        int money
) {
}
