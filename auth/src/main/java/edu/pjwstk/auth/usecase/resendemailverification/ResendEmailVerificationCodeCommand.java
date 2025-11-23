package edu.pjwstk.auth.usecase.resendemailverification;

import edu.pjwstk.core.Command;

import java.util.UUID;

public record ResendEmailVerificationCodeCommand(UUID userId) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new IllegalArgumentException("User ID cannot be null");
        }
    }
}
