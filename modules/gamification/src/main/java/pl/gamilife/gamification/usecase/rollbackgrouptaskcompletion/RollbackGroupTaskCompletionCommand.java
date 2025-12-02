package pl.gamilife.gamification.usecase.rollbackgrouptaskcompletion;

import pl.gamilife.infrastructure.core.architecture.Command;
import jakarta.validation.ValidationException;

import java.util.UUID;

public record RollbackGroupTaskCompletionCommand(UUID userId) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("User id cannot be null");
        }
    }
}
