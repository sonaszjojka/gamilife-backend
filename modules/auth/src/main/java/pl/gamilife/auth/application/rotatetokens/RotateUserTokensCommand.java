package pl.gamilife.auth.application.rotatetokens;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import pl.gamilife.api.auth.dto.RotateUserTokensDto;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record RotateUserTokensCommand(
        @NotNull
        UUID userId,

        @NotBlank
        @Email
        String email,

        boolean isEmailVerified
) implements Command {
    public static RotateUserTokensCommand from(RotateUserTokensDto dto) {
        return new RotateUserTokensCommand(
                dto.userId(),
                dto.email(),
                dto.isEmailVerified()
        );
    }
}
