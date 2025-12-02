package edu.pjwstk.user.dto.service;

import java.util.UUID;

public record ChangeUserPasswordCommand(
        UUID userId,
        String oldPassword,
        String newPassword
) {
}
