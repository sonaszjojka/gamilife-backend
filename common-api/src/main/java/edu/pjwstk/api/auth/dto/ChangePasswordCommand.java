package edu.pjwstk.api.auth.dto;

public record ChangePasswordCommand(
        String providedPassword,
        String hashedUserPassword,
        String newPassword
) {
}
