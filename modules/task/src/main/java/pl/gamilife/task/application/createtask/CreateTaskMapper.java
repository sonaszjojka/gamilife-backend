package pl.gamilife.task.application.createtask;

import pl.gamilife.task.entity.Task;
import pl.gamilife.task.entity.TaskCategory;
import pl.gamilife.task.entity.TaskDifficulty;

import java.util.UUID;

public interface CreateTaskMapper {

    Task toEntity(CreateTaskRequest req,
                  UUID taskId,
                  TaskCategory category,
                  TaskDifficulty difficulty,
                  UUID userId);

    CreateTaskResponse toResponse(Task task);
}
