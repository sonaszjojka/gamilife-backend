package edu.pjwstk.auth.usecase.command;

import edu.pjwstk.auth.validators.SecurePassword;

public record ChangePasswordInternalCommand(
        String providedPassword,
        String hashedUserPassword,

        @SecurePassword
        String newPassword
) {
}
