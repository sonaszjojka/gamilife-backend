package edu.pjwstk.auth.usecase.sendforgotpasswordcode;

import edu.pjwstk.core.Command;

public record SendForgotPasswordCodeCommand(String email) implements Command {
    @Override
    public void validate() {
        if (email == null || email.isBlank()) {
            throw new IllegalArgumentException("Email cannot be null or blank");
        }
    }
}
