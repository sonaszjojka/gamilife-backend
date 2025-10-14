package edu.pjwstk.common.userApi.dto;

import java.util.UUID;

public record SecureUserInfoApiDto(
        UUID userId, String email, String password, boolean isEmailVerified
) {
}
