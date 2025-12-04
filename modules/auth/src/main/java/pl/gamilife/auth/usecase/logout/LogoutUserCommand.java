package pl.gamilife.auth.usecase.logout;

import jakarta.validation.ValidationException;
import pl.gamilife.infrastructure.core.architecture.Command;

public record LogoutUserCommand(String refreshToken) implements Command {
    @Override
    public void validate() {
        if (refreshToken == null || refreshToken.isBlank()) {
            throw new ValidationException("Refresh token cannot be blank");
        }
    }
}
