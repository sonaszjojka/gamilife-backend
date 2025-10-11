package edu.pjwstk.auth.dto.request;

import jakarta.validation.constraints.NotBlank;

public record OAuthCodeRequest(
        @NotBlank(message = "provider is required")
        String code,

        @NotBlank(message = "codeVerifier is required")
        String codeVerifier
) {
}
