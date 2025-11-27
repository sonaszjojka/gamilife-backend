package edu.pjwstk.gamification.usecase.processpomodorotaskcompletion;

import edu.pjwstk.core.Command;
import jakarta.validation.ValidationException;

import java.util.UUID;

public record ProcessPomodoroTaskCompletionCommand(UUID userId, boolean rewardGranted) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("User id cannot be null");
        }
    }
}
