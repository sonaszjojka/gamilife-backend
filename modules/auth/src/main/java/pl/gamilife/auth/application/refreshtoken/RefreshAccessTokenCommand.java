package pl.gamilife.auth.application.refreshtoken;

import jakarta.validation.constraints.NotBlank;
import pl.gamilife.shared.kernel.architecture.Command;

public record RefreshAccessTokenCommand(@NotBlank String refreshToken) implements Command {
}
