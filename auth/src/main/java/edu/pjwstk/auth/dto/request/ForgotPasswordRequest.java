package edu.pjwstk.auth.dto.request;

import jakarta.validation.constraints.Email;

public record ForgotPasswordRequest(@Email String email) {
}
