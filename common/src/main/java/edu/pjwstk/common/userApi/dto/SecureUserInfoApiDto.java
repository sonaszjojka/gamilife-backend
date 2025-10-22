package edu.pjwstk.common.userApi.dto;

import java.time.Instant;
import java.util.UUID;

public record SecureUserInfoApiDto(
        UUID userId, String email, String password, Instant passwordChangeDate, boolean isEmailVerified
) {
}
