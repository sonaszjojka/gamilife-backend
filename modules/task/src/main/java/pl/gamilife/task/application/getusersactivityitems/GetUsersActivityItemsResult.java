package pl.gamilife.task.application.getusersactivityitems;

import pl.gamilife.shared.kernel.enums.ActivityType;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.UUID;

public record GetUsersActivityItemsResult(
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
        ActivityStatus status
) {
    public enum ActivityStatus {
        ALIVE, INCOMPLETE, DEADLINE_TODAY, DEADLINE_MISSED
    }
}
