package edu.pjwstk.auth.dto.service;

import java.util.UUID;

public record LinkOAuthAccountDto(
        boolean shouldLink,
        String provider,
        String providerId,
        UUID userId,
        String password
) {

}
