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
        if (userId == null) {
            throw new IllegalArgumentException("User ID cannot be null");
        }

        if (email == null || email.isBlank()) {
            throw new IllegalArgumentException("Email cannot be null or blank");
        }
    }
}
