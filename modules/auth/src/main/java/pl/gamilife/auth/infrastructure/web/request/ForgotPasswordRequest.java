package pl.gamilife.auth.infrastructure.web.request;

import jakarta.validation.constraints.Email;

public record ForgotPasswordRequest(@Email String email) {
}
