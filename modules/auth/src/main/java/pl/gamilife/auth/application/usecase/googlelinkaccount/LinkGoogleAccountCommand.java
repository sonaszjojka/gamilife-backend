package pl.gamilife.auth.application.usecase.googlelinkaccount;

import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record LinkGoogleAccountCommand(
        boolean shouldLink,
        String provider,
        String providerId,
        UUID userId,
        String password
) implements Command {
}
