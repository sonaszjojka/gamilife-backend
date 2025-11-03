package edu.pjwstk.api.auth.dto;

import java.util.UUID;

public record CurrentUserDto(
        UUID userId,
        String email
) {
}
