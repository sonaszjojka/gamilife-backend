package pl.gamilife.auth.controller.request;

import jakarta.validation.constraints.NotBlank;

public record EmailVerificationCodeRequest(@NotBlank String code) {
}
