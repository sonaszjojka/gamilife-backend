package edu.pjwstk.auth.dto.request;

import jakarta.validation.constraints.NotNull;

import java.util.UUID;

public record LinkOAuthAccountRequest(
        @NotNull(message = "shouldLink is required")
        boolean shouldLink,

        String provider,

        String providerId,

        UUID userId,

        String password
) {
        public boolean validate() {
                return !shouldLink || provider != null && providerId != null && userId != null && password != null;
        }
}
