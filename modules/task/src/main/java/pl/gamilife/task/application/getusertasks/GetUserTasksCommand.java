package pl.gamilife.task.application.getusertasks;

import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record GetUserTasksCommand(
        UUID userId,
        Integer categoryId,
        Integer difficultyId,
        Boolean isCompleted,
        Boolean isGroupTask,
        Integer pageNumber,
        Integer pageSize
) implements Command {
    @Override
    public void validate() {

    }
}
