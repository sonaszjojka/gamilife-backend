package pl.gamilife.auth.usecase.changepassword;

import pl.gamilife.api.auth.dto.ChangePasswordDto;
import pl.gamilife.infrastructure.core.architecture.Command;
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
