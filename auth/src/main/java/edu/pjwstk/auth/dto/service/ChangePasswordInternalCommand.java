package edu.pjwstk.auth.dto.service;

import edu.pjwstk.auth.validators.SecurePassword;

public record ChangePasswordInternalCommand(
        String providedPassword,
        String hashedUserPassword,

        @SecurePassword
        String newPassword
) {
}
