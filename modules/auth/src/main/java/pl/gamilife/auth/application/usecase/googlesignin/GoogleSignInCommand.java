package pl.gamilife.auth.application.usecase.googlesignin;

import jakarta.validation.constraints.NotBlank;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.ZoneId;

public record GoogleSignInCommand(
        @NotBlank
        String code,

        @NotBlank
        String codeVerifier,

        ZoneId zoneId
) implements Command {
}
