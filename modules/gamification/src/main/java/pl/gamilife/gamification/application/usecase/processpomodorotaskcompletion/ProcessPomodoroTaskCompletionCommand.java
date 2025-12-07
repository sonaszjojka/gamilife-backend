package pl.gamilife.gamification.application.usecase.processpomodorotaskcompletion;

import jakarta.validation.ValidationException;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record ProcessPomodoroTaskCompletionCommand(UUID userId, boolean rewardGranted) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("User id cannot be null");
        }
    }
}
