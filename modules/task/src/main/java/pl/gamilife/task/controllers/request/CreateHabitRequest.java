package pl.gamilife.task.controllers.request;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.PositiveOrZero;

import java.time.Duration;

public record CreateHabitRequest(
        @NotNull(message = "Cycle length cannot be null")
        Duration cycleLength
) {
}
