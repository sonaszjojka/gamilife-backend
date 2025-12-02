package edu.pjwstk.gamification.usecase.processtaskcompletion;

import pl.gamilife.infrastructure.core.architecture.Command;
import jakarta.validation.ValidationException;

import java.util.UUID;

public record ProcessTaskCompletionCommand(UUID userId, boolean rewardGranted) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("User id cannot be null");
        }
    }
}
