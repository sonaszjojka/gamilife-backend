package pl.gamilife.auth.application.usecase.login;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import pl.gamilife.shared.kernel.architecture.Command;

public record LoginUserCommand(
        @NotBlank
        @Email
        String email,

        @NotBlank
        String password
) implements Command {
}
