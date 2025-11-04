package edu.pjwstk.auth.usecase.command;

import edu.pjwstk.core.Command;

public record HandleGoogleSignInCommand(
        String code,
        String codeVerifier
) implements Command {
}
