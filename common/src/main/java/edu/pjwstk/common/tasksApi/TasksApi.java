package edu.pjwstk.common.tasksApi;

import edu.pjwstk.common.tasksApi.dto.TaskDto;

import java.util.UUID;

public interface TasksApi {
    Boolean taskExistsByTaskId(UUID taskId);

    TaskDto findTaskByTaskId(UUID taskId);

}
