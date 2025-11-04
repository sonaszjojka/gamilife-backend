package edu.pjwstk.auth.usecase.verifyemail;

import edu.pjwstk.core.Command;

import java.util.UUID;

public record VerifyEmailCommand(
        UUID userId,
        String code
) implements Command {
}
