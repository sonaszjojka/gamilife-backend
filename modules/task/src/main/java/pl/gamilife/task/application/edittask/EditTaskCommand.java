package pl.gamilife.task.application.edittask;

import jakarta.validation.constraints.FutureOrPresent;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.Instant;
import java.util.UUID;

public record EditTaskCommand(
        @NotNull
        UUID userId,

        @NotNull
        UUID taskId,

        @Size(min = 1, max = 200, message = "Title cannot exceed 200 characters")
        String title,

        @FutureOrPresent
        Instant deadline,

        Integer categoryId,

        Integer difficultyId,

        Boolean completed,

        @Size(min = 1, max = 500, message = "Description cannot exceed 500 characters")
        String description
) implements Command {
    @Override
    public void validate() {

    }
}
