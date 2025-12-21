package pl.gamilife.auth.application.logout;

import jakarta.validation.ValidationException;
import pl.gamilife.shared.kernel.architecture.Command;

public record LogoutUserCommand(String refreshToken) implements Command {
    @Override
    public void validate() {
        if (refreshToken == null || refreshToken.isBlank()) {
            throw new ValidationException("Refresh token cannot be blank");
        }
    }
}
