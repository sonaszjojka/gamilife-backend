package edu.pjwstk.pomodoro.createpomodorotask;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;

import java.util.UUID;

public record CreatePomodoroTaskRequest(
        @NotNull(message = "Work cycles needed cannot be null")
        @Positive(message = "Work cycles needed streak must be positive")
        Integer workCyclesNeeded,

        @NotNull(message = "Work cycles completed cannot be null")
        @PositiveOrZero(message = "Work cycles completed streak must be positive")
        Integer workCyclesCompleted
) {
}
