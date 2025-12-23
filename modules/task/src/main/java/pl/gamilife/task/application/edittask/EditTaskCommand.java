package pl.gamilife.task.application.edittask;

import jakarta.validation.ValidationException;
import jakarta.validation.constraints.FutureOrPresent;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.UUID;

public record EditTaskCommand(
        @NotNull
        UUID userId,

        ZoneId zoneId,

        @NotNull
        UUID taskId,

        @Size(min = 1, max = 200, message = "Title cannot exceed 200 characters")
        String title,

        Boolean removeDescription,

        @Size(min = 1, max = 500, message = "Description cannot exceed 500 characters")
        String description,

        @FutureOrPresent
        LocalDate deadlineDate,

        Boolean removeDeadlineTime,

        LocalTime deadlineTime,

        Integer categoryId,

        Integer difficultyId,

        Boolean completed
) implements Command {
    @Override
    public void validate() {
        if (Boolean.TRUE.equals(removeDescription) && description != null) {
            throw new ValidationException("Detected inconsistency in removeDescription and description fields");
        }

        if (Boolean.TRUE.equals(removeDeadlineTime) && deadlineTime != null) {
            throw new ValidationException("Detected inconsistency in removeDeadlineTime and deadlineTime fields");
        }
    }
}
