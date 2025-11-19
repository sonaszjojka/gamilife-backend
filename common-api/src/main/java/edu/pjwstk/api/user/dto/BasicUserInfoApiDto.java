package edu.pjwstk.api.user.dto;

import java.util.UUID;

public record BasicUserInfoApiDto(
        UUID userId,
        String email,
        String username,
        Integer experience,
        Integer money
) {
}
