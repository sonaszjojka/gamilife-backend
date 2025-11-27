package edu.pjwstk.user.usecase.levelupuser;

import edu.pjwstk.core.Command;
import jakarta.validation.ValidationException;

import java.util.UUID;

public record LevelUpUserCommand(UUID userId, int level) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("User id cannot be null");
        }

        if (level <= 0) {
            throw new ValidationException("Level must be positive");
        }
    }
}
