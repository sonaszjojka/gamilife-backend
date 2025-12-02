package edu.pjwstk.auth.usecase.refreshtoken;

import pl.gamilife.infrastructure.core.architecture.Command;

public record RefreshAccessTokenCommand(String refreshToken) implements Command {
    @Override
    public void validate() {
        if (refreshToken == null || refreshToken.isBlank()) {
            throw new IllegalArgumentException("Refresh token cannot be blank");
        }
    }
}
