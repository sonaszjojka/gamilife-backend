package pl.gamilife.auth.application.changepassword;

import jakarta.validation.ValidationException;
import pl.gamilife.api.auth.dto.ChangePasswordDto;
import pl.gamilife.shared.kernel.architecture.Command;

public record ChangePasswordCommand(
        String providedPassword,
        String hashedUserPassword,
        String newPassword
) implements Command {
    public static ChangePasswordCommand from(ChangePasswordDto dto) {
        return new ChangePasswordCommand(
                dto.providedPassword(),
                dto.hashedUserPassword(),
                dto.newPassword()
        );
    }

    @Override
    public void validate() {
        if (newPassword == null || newPassword.isBlank()) {
            throw new ValidationException("New password cannot be blank");
        }

        if (providedPassword == null || providedPassword.isBlank()) {
            throw new ValidationException("Provided password cannot be blank");
        }

        if (hashedUserPassword == null || hashedUserPassword.isBlank()) {
            throw new ValidationException("Hashed user password cannot be blank");
        }
    }
}
