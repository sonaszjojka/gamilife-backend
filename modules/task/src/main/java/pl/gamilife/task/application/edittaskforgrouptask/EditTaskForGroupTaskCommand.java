package pl.gamilife.task.application.edittaskforgrouptask;

import jakarta.validation.constraints.FutureOrPresent;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.Instant;
import java.util.UUID;

public record EditTaskForGroupTaskCommand(
        @NotNull
        UUID taskId,

        @Size(min = 1, max = 200)
        String title,

        @FutureOrPresent
        Instant deadline,

        Integer categoryId,

        Integer difficultyId,

        Boolean completed,

        @Size(min = 1, max = 500)
        String description
) implements Command {
    @Override
    public void validate() {

    }
}
