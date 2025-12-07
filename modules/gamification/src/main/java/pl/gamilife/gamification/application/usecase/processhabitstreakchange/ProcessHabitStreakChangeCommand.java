package pl.gamilife.gamification.application.usecase.processhabitstreakchange;

import jakarta.validation.ValidationException;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record ProcessHabitStreakChangeCommand(UUID userId, int streakValue) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("User id cannot be null");
        }

        if (streakValue < 0) {
            throw new ValidationException("Streak value cannot be negative");
        }
    }
}
