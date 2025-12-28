package pl.gamilife.app.dto.activity;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.UUID;

public record ActivityItemDetails(
        UUID id,
        ActivityType type,
        String title,
        String description,
        Integer categoryId,
        String categoryName,
        Integer difficultyId,
        String difficultyName,
        LocalDate deadlineDate,
        LocalTime deadlineTime,
        Integer cycleLength,
        Integer currentStreak,
        Integer longestStreak,
        ActivityStatus status,
        Boolean canBeWorkedOn,
        PomodoroDto pomodoro
) {
    public enum ActivityType {
        TASK, HABIT;

        public static ActivityType from(pl.gamilife.shared.kernel.enums.ActivityType type) {
            return switch (type) {
                case TASK -> ActivityType.TASK;
                case HABIT -> ActivityType.HABIT;
            };
        }
    }

    public enum ActivityStatus {
        ALIVE, INCOMPLETE, DEADLINE_TODAY, DEADLINE_MISSED
    }

    public record PomodoroDto(UUID id, int cyclesRequired, int cyclesCompleted) {
    }
}
