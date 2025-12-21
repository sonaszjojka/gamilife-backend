package pl.gamilife.auth.application.resetpassword;

import pl.gamilife.shared.kernel.architecture.Command;

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
