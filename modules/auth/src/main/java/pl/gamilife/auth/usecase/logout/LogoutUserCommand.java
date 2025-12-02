package pl.gamilife.auth.usecase.logout;

import pl.gamilife.infrastructure.core.architecture.Command;
import jakarta.validation.ValidationException;

public record LogoutUserCommand(String refreshToken) implements Command {
    @Override
    public void validate() {
        if (refreshToken == null || refreshToken.isBlank()) {
            throw new ValidationException("Refresh token cannot be blank");
        }
    }
}
