package pl.gamilife.auth.application.usecase.logout;

import jakarta.validation.constraints.NotBlank;
import pl.gamilife.shared.kernel.architecture.Command;

public record LogoutUserCommand(@NotBlank String refreshToken) implements Command {
}
