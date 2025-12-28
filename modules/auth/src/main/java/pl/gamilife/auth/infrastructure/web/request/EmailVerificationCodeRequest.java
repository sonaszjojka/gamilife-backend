package pl.gamilife.auth.infrastructure.web.request;

import jakarta.validation.constraints.NotBlank;

public record EmailVerificationCodeRequest(@NotBlank String code) {
}
