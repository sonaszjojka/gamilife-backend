package pl.gamilife.auth.application.changepassword;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record ChangePasswordCommand(
        @NotNull
        UUID userId,

        @NotBlank
        String providedPassword,

        @NotBlank
        String newPassword
) implements Command {
}
