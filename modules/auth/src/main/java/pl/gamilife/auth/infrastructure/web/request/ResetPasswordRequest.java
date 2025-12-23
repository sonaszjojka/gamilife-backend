package pl.gamilife.auth.infrastructure.web.request;

import jakarta.validation.constraints.NotBlank;

public record ResetPasswordRequest(
        @NotBlank
        String code,

        String newPassword
) {
}
