package pl.gamilife.auth.application.resendemailverification;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record ResendEmailVerificationCodeCommand(@NotNull UUID userId) implements Command {
}
