package edu.pjwstk.gamification.usecase.processhabitstreakchange;

import edu.pjwstk.core.Command;
import jakarta.validation.ValidationException;

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
