package edu.pjwstk.common.authApi.dto;

import java.util.UUID;

public record CurrentUserDto(
        UUID userId,
        String email
) {
}
