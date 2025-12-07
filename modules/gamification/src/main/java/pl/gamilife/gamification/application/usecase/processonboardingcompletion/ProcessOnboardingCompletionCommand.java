package pl.gamilife.gamification.application.usecase.processonboardingcompletion;

import jakarta.validation.ValidationException;
import pl.gamilife.infrastructure.core.architecture.Command;

import java.util.UUID;

public record ProcessOnboardingCompletionCommand(UUID userId) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("userId cannot be null");
        }
    }
}
