package pl.gamilife.auth.controller.request;

import jakarta.validation.constraints.NotBlank;

public record ResetPasswordRequest(
        @NotBlank
        String code,

        String newPassword
) {
}
