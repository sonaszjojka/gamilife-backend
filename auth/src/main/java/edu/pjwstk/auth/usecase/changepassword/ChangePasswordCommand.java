package edu.pjwstk.auth.usecase.changepassword;

import edu.pjwstk.api.auth.dto.ChangePasswordDto;
import edu.pjwstk.core.Command;
import jakarta.validation.ValidationException;

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
