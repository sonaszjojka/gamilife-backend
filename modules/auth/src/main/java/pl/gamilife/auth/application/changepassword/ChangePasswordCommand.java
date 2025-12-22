package pl.gamilife.auth.application.changepassword;

import jakarta.validation.constraints.NotBlank;
import pl.gamilife.api.auth.dto.ChangePasswordDto;
import pl.gamilife.shared.kernel.architecture.Command;

public record ChangePasswordCommand(
        @NotBlank
        String providedPassword,

        @NotBlank
        String hashedUserPassword,

        @NotBlank
        String newPassword
) implements Command {
    public static ChangePasswordCommand from(ChangePasswordDto dto) {
        return new ChangePasswordCommand(
                dto.providedPassword(),
                dto.hashedUserPassword(),
                dto.newPassword()
        );
    }
}
