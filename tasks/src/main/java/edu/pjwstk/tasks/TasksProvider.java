package edu.pjwstk.tasks;

import edu.pjwstk.tasks.entity.Task;

import java.util.Optional;
import java.util.UUID;

public interface TasksProvider {
    Boolean taskExistsByTaskId(UUID taskId);

    Task findTaskByTaskId(UUID taskId);

}
