package edu.pjwstk.auth.usecase.rotatetokens;

import edu.pjwstk.api.auth.dto.RotateUserTokensDto;
import edu.pjwstk.core.Command;

import java.util.UUID;

public record RotateUserTokensCommand(
        UUID userId,
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

    @Override
    public void validate() {

    }
}
