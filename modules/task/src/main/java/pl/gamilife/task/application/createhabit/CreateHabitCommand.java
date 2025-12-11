package pl.gamilife.task.application.createhabit;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record CreateHabitCommand(
        @NotNull
        UUID userId,

        @Size(min = 1, max = 200, message = "Title cannot exceed 200 characters")
        String title,

        @Size(min = 1, max = 500, message = "Description cannot exceed 500 characters")
        String description,

        @NotNull(message = "Category Id cannot be null")
        Integer categoryId,

        @NotNull(message = "Difficulty Id cannot be null")
        Integer difficultyId,

        @NotNull(message = "Cycle length cannot be null")
        @Positive
        Integer cycleLength
) implements Command {
}
