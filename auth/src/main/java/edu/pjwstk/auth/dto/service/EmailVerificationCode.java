package edu.pjwstk.auth.dto.service;

import java.util.UUID;

public record EmailVerificationCode(
        UUID userId,
        String code
) {
}
