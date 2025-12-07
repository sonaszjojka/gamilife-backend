package pl.gamilife.auth.usecase.resendemailverification;

import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record ResendEmailVerificationCodeCommand(UUID userId) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new IllegalArgumentException("User ID cannot be null");
        }
    }
}
