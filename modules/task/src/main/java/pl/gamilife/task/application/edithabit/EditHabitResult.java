package pl.gamilife.task.application.edithabit;

import java.time.LocalDate;
import java.util.UUID;

public record EditHabitResult(
        UUID habitId,
        LocalDate deadlineDate,
        Integer cycleLength,
        Integer currentStreak,
        Integer longestStreak,
        boolean workable,
        HabitStatus status
) {
    public enum HabitStatus {
        ALIVE, DEAD
    }
}
