package pl.gamilife.task.application.getusershabits;

import java.time.LocalDate;
import java.util.UUID;

public record GetUsersHabitsResult(
        UUID habitId,
        String title,
        String description,
        LocalDate currentDeadline,
        Integer categoryId,
        String categoryName,
        Integer difficultyId,
        String difficultyName,
        Integer cycleLength,
        Integer currentStreak,
        Integer longestStreak,
        Boolean isAlive
) {
}
