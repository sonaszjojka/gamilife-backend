package pl.gamilife.auth.application.resetpassword;

import jakarta.validation.constraints.NotBlank;
import pl.gamilife.shared.kernel.architecture.Command;

public record ResetPasswordCommand(
        @NotBlank
        String code,

        @NotBlank
        String newPassword
) implements Command {
}
