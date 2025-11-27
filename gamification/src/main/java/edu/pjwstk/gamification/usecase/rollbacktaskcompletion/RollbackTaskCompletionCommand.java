package edu.pjwstk.gamification.usecase.rollbacktaskcompletion;

import edu.pjwstk.core.Command;
import jakarta.validation.ValidationException;

import java.util.UUID;

public record RollbackTaskCompletionCommand(UUID userId) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("User id cannot be null");
        }
    }
}
