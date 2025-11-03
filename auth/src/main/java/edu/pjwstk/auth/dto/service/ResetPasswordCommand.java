package edu.pjwstk.auth.dto.service;

import edu.pjwstk.auth.validators.SecurePassword;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

public record ResetPasswordCommand(
        @NotBlank
        String code,

        @NotNull
        @SecurePassword
        String newPassword
) {
}
