package edu.pjwstk.user.dto.request;

public record ChangeUserPasswordRequest(
        String oldPassword,
        String newPassword
) {
}
