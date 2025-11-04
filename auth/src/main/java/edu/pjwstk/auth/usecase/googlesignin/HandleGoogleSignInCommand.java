package edu.pjwstk.auth.usecase.googlesignin;

import edu.pjwstk.core.Command;

public record HandleGoogleSignInCommand(
        String code,
        String codeVerifier
) implements Command {
}
