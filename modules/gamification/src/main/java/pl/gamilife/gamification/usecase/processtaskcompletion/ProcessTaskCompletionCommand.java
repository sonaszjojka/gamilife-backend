package pl.gamilife.gamification.usecase.processtaskcompletion;

import jakarta.validation.ValidationException;
import pl.gamilife.infrastructure.core.architecture.Command;

import java.util.UUID;

public record ProcessTaskCompletionCommand(UUID userId, boolean rewardGranted) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("User id cannot be null");
        }
    }
}
