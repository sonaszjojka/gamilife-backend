package pl.gamilife.auth.infrastructure.web.request;

import jakarta.validation.constraints.NotBlank;

public record ChangeUserPasswordRequest(
        @NotBlank
        String oldPassword,

        @NotBlank
        String newPassword
) {
}
