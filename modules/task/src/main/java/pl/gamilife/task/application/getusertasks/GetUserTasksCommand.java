package pl.gamilife.task.application.getusertasks;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.ZoneId;
import java.util.UUID;

public record GetUserTasksCommand(
        @NotNull
        UUID userId,

        ZoneId zoneId,

        Integer categoryId,
        Integer difficultyId,
        Boolean isCompleted,

        @NotNull
        Boolean isGroupTask,
        Integer pageNumber,
        Integer pageSize
) implements Command {
}
