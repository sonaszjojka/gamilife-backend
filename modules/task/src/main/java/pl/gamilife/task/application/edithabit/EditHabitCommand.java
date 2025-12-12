package pl.gamilife.task.application.edithabit;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.ZoneId;
import java.util.UUID;

public record EditHabitCommand(
        @NotNull
        UUID userId,

        ZoneId zoneId,

        @NotNull
        UUID habitId,

        @Size(max = 200, message = "Title cannot exceed 200 characters")
        String title,

        @Size(min = 1, max = 200, message = "Description cannot exceed 500 characters")
        String description,

        Integer categoryId,

        Integer difficultyId,

        @Positive
        Integer cycleLength,

        Boolean iterationCompleted,

        Boolean finished
) implements Command {
}
