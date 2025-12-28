package pl.gamilife.gamification.application.usecase.processhabitstreakchange;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record ProcessHabitStreakChangeCommand(
        @NotNull
        UUID userId,

        @Positive
        int streakValue
) implements Command {
}
