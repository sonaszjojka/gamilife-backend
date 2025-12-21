package pl.gamilife.auth.application.sendforgotpasswordcode;

import pl.gamilife.shared.kernel.architecture.Command;

public record SendForgotPasswordCodeCommand(String email) implements Command {
    @Override
    public void validate() {
        if (email == null || email.isBlank()) {
            throw new IllegalArgumentException("Email cannot be null or blank");
        }
    }
}
