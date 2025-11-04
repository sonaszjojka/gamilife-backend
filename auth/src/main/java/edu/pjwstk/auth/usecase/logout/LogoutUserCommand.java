package edu.pjwstk.auth.usecase.logout;

import edu.pjwstk.core.Command;
import jakarta.validation.ValidationException;

public record LogoutUserCommand(String refreshToken) implements Command {
    @Override
    public void validate() {
        if (refreshToken.isBlank()) {
            throw new ValidationException("Refresh token cannot be blank");
        }
    }
}
