package edu.pjwstk.auth.usecase.googlesignin;

import edu.pjwstk.core.Command;

public record GoogleSignInCommand(
        String code,
        String codeVerifier
) implements Command {
    @Override
    public void validate() {

    }
}
