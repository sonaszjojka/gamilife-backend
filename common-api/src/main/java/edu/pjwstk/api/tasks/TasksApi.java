package edu.pjwstk.api.tasks;

import edu.pjwstk.api.tasks.dto.TaskDto;
import edu.pjwstk.api.tasks.dto.TaskForGroupTaskRequestDto;
import edu.pjwstk.api.tasks.dto.TaskForGroupTaskResponseDto;

import java.util.UUID;

public interface TasksApi {
    Boolean taskExistsByTaskId(UUID taskId);

    TaskDto findTaskByTaskId(UUID taskId);

    void deleteTaskByTaskId(UUID taskId);

    TaskForGroupTaskResponseDto createTaskForGroupTask(TaskForGroupTaskRequestDto request);

}
