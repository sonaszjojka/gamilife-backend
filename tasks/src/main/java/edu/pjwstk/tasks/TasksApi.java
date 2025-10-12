package edu.pjwstk.tasks;

import edu.pjwstk.tasks.shared.TaskDto;

import java.util.UUID;

public interface TasksApi {
    Boolean taskExistsByTaskId(UUID taskId);

    TaskDto findTaskByTaskId(UUID taskId);

}
