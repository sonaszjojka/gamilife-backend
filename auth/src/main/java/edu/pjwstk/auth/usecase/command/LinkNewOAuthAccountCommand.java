package edu.pjwstk.auth.usecase.command;

import edu.pjwstk.core.Command;

import java.util.UUID;

public record LinkNewOAuthAccountCommand(
        boolean shouldLink,
        String provider,
        String providerId,
        UUID userId,
        String password
) implements Command {

}
