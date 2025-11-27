package edu.pjwstk.tasks.application.createhabit;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.PositiveOrZero;

import java.time.Duration;
import java.time.LocalDateTime;

public record CreateHabitRequest(
        @NotNull(message = "Cycle length cannot be null")
        Duration cycleLength,

        @NotNull(message = "Current streak cannot be null")
        @PositiveOrZero(message = "Current streak must be positive")
        Integer currentStreak,

        @NotNull(message = "Longest streak cannot be null")
        @PositiveOrZero(message = "Longest streak must be positive")
        Integer longestStreak,

        LocalDateTime acceptedDate


) {
}
