package pl.gamilife.api.task.dto;

import pl.gamilife.shared.kernel.enums.ActivityType;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.UUID;

public record ActivityItemDto(
        UUID id,
        ActivityType type,
        String title,
        String description,
        UUID userId,
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
        Boolean canBeWorkedOn
) {
    public enum ActivityStatus {
        ALIVE, INCOMPLETE, DEADLINE_TODAY, DEADLINE_MISSED
    }
}
