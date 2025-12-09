package pl.gamilife.task.application.edithabit;

import jakarta.validation.constraints.NotNull;

import java.time.Duration;
import java.time.Instant;

public record EditHabitRequest(
        @NotNull(message = "Cycle length cannot be null")
        Duration cycleLength,
        Boolean finished
) {
}
