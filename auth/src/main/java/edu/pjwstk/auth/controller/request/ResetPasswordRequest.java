package edu.pjwstk.auth.controller.request;

import edu.pjwstk.auth.validators.SecurePassword;
import jakarta.validation.constraints.NotBlank;

public record ResetPasswordRequest(
        @NotBlank
        String code,

        @SecurePassword
        String newPassword
) {
}
