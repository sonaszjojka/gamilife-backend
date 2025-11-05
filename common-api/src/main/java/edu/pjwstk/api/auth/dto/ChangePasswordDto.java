package edu.pjwstk.api.auth.dto;

public record ChangePasswordDto(
        String providedPassword,
        String hashedUserPassword,
        String newPassword
) {
}
