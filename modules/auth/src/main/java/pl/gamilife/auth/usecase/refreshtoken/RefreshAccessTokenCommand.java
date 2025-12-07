package pl.gamilife.auth.usecase.refreshtoken;

import pl.gamilife.shared.kernel.architecture.Command;

public record RefreshAccessTokenCommand(String refreshToken) implements Command {
    @Override
    public void validate() {
        if (refreshToken == null || refreshToken.isBlank()) {
            throw new IllegalArgumentException("Refresh token cannot be blank");
        }
    }
}
