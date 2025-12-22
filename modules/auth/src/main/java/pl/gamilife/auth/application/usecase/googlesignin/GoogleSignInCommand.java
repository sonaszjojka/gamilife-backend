package pl.gamilife.auth.application.usecase.googlesignin;

import jakarta.validation.constraints.NotBlank;
import pl.gamilife.shared.kernel.architecture.Command;

public record GoogleSignInCommand(
        @NotBlank
        String code,

        @NotBlank
        String codeVerifier
) implements Command {
}
