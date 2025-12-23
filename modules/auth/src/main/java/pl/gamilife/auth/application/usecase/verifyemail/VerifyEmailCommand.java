package pl.gamilife.auth.application.usecase.verifyemail;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record VerifyEmailCommand(
        @NotNull
        UUID userId,

        @NotBlank
        String code
) implements Command {
}
