package edu.pjwstk.tasks.createtask;

import edu.pjwstk.tasks.domain.Habit;
import edu.pjwstk.tasks.domain.Task;
import edu.pjwstk.tasks.domain.TaskCategory;
import edu.pjwstk.tasks.domain.TaskDifficulty;

import java.util.UUID;

public interface CreateTaskMapper {

    Task toEntity(CreateTaskRequest req,
                  UUID taskId,
                  TaskCategory category,
                  TaskDifficulty difficulty,
                  Habit habit,
                  Task previousHabitTask);

    CreateTaskResponse toResponse(Task task);
}
