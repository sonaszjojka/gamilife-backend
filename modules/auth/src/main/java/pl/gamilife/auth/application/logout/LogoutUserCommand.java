package pl.gamilife.auth.application.logout;

import jakarta.validation.constraints.NotBlank;
import pl.gamilife.shared.kernel.architecture.Command;

public record LogoutUserCommand(@NotBlank String refreshToken) implements Command {
}
