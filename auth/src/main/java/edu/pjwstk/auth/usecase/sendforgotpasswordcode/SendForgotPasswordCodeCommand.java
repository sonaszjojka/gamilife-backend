package edu.pjwstk.auth.usecase.sendforgotpasswordcode;

import edu.pjwstk.core.Command;

public record SendForgotPasswordCodeCommand(String email) implements Command {
    @Override
    public void validate() {
        // No specific validation logic
    }
}
