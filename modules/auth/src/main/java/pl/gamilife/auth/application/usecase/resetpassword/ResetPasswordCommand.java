package pl.gamilife.auth.application.usecase.resetpassword;

import jakarta.validation.constraints.NotBlank;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record ResetPasswordCommand(
        @NotBlank
        String code,

        @NotBlank
        String newPassword,

        UUID authenticatedUserId
) implements Command {
}
