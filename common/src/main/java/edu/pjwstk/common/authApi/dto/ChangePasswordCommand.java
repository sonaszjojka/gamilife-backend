package edu.pjwstk.common.authApi.dto;

public record ChangePasswordCommand(
        String providedPassword,
        String hashedUserPassword,
        String newPassword
) {
}
