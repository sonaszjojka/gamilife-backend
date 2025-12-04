package pl.gamilife.auth.controller.request;

import jakarta.validation.constraints.Email;

public record ForgotPasswordRequest(@Email String email) {
}
