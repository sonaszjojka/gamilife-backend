package pl.gamilife.auth.usecase.googlelinkaccount;

import pl.gamilife.infrastructure.core.architecture.Command;

import java.util.UUID;

public record LinkGoogleAccountCommand(
        boolean shouldLink,
        String provider,
        String providerId,
        UUID userId,
        String password
) implements Command {

    @Override
    public void validate() {

    }
}
