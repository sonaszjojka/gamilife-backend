package edu.pjwstk.auth.usecase.resetpassword;

import edu.pjwstk.core.Command;

public record ResetPasswordCommand(String code, String newPassword) implements Command {
    @Override
    public void validate() {
        if (code == null || code.isBlank()) {
            throw new IllegalArgumentException("Code cannot be blank");
        }

        if (newPassword == null || newPassword.isBlank()) {
            throw new IllegalArgumentException("New password cannot be null or blank");
        }
    }
}
