package edu.pjwstk.auth.usecase.changepassword;

import edu.pjwstk.auth.validators.SecurePassword;

public record ChangePasswordInternalCommand(
        String providedPassword,
        String hashedUserPassword,

        @SecurePassword
        String newPassword
) {
}
