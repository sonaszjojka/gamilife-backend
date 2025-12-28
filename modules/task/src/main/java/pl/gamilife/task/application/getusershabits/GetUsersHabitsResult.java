package pl.gamilife.task.application.getusershabits;

import java.time.LocalDate;
import java.util.UUID;

public record GetUsersHabitsResult(
        UUID id,
        HabitType type,
        String title,
        String description,
        LocalDate deadlineDate,
        Integer categoryId,
        String categoryName,
        Integer difficultyId,
        String difficultyName,
        Integer cycleLength,
        Integer currentStreak,
        Integer longestStreak,
        HabitStatus status
) {
    public enum HabitStatus {
        ALIVE, DEAD
    }

    public enum HabitType {
        HABIT
    }
}
