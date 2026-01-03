package pl.gamilife.task.application.getusertasks;


import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.UUID;

public record GetUserTasksResult(
        UUID id,
        TaskType type,
        String title,
        String description,
        LocalDate deadlineDate,
        LocalTime deadlineTime,
        Integer categoryId,
        Integer difficultyId,
        TaskStatus status,
        String categoryName,
        String difficultyName,
        Boolean isGroupTask,
        Instant completedAt
) {
    public enum TaskStatus {
        COMPLETED, INCOMPLETE, DEADLINE_TODAY, DEADLINE_MISSED
    }

    public enum TaskType {
        TASK
    }
}
