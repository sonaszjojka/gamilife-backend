package pl.gamilife.auth.usecase.sendforgotpasswordcode;

import pl.gamilife.infrastructure.core.architecture.Command;

public record SendForgotPasswordCodeCommand(String email) implements Command {
    @Override
    public void validate() {
        if (email == null || email.isBlank()) {
            throw new IllegalArgumentException("Email cannot be null or blank");
        }
    }
}
