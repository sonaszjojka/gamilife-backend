package pl.gamilife.task.infrastructure.web.request;

import jakarta.validation.constraints.NotNull;

import java.time.Duration;

public record CreateHabitRequest(
        @NotNull(message = "Cycle length cannot be null")
        Duration cycleLength
) {
}
