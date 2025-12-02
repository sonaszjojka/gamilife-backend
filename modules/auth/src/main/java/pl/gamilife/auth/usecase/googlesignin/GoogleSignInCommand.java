package pl.gamilife.auth.usecase.googlesignin;

import pl.gamilife.infrastructure.core.architecture.Command;

public record GoogleSignInCommand(
        String code,
        String codeVerifier
) implements Command {
    @Override
    public void validate() {
        if (code == null || code.isBlank()) {
            throw new IllegalArgumentException("Code must not be null or blank");
        }

        if (codeVerifier == null || codeVerifier.isBlank()) {
            throw new IllegalArgumentException("Code verifier must not be null or blank");
        }
    }
}
