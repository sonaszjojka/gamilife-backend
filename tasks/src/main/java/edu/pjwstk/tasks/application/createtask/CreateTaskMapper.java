package edu.pjwstk.tasks.application.createtask;

import edu.pjwstk.tasks.entity.Habit;
import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.entity.TaskCategory;
import edu.pjwstk.tasks.entity.TaskDifficulty;

import java.util.UUID;

public interface CreateTaskMapper {

    Task toEntity(CreateTaskRequest req,
                  UUID taskId,
                  TaskCategory category,
                  TaskDifficulty difficulty,
                  UUID userId);

    CreateTaskResponse toResponse(Task task);
}
