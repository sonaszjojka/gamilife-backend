package edu.pjwstk.tasks.application.createhabit;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.UUID;

public record CreateHabitRequest(
        @NotNull(message = "Habit Task Id cannot be null")
        UUID taskId,

        @NotNull(message = "Cycle length cannot be null")
        Duration cycleLength,

        @NotNull(message = "Current streak cannot be null")
        @Positive(message = "Current streak must be positive")
        Integer currentStreak,

        @NotNull(message = "Longest streak cannot be null")
        @Positive(message = "Longest streak must be positive")
        Integer longestStreak,

        @NotNull(message = "isAccepted cannot be null")
        Boolean isAccepted,

        LocalDateTime acceptedDate,

        @Size(max = 300, message = "Decline message cannot exceed 300 characters")
        String declineMessage

) {
}
