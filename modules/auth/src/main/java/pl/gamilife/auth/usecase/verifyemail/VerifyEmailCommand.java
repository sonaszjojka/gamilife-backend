package pl.gamilife.auth.usecase.verifyemail;

import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record VerifyEmailCommand(
        UUID userId,
        String code
) implements Command {

    @Override
    public void validate() {
        if (userId == null) {
            throw new IllegalArgumentException("User id cannot be null");
        }

        if (code == null || code.isBlank()) {
            throw new IllegalArgumentException("Code cannot be null or blank");
        }
    }
}
