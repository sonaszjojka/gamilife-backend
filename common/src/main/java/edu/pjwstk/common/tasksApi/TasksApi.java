package edu.pjwstk.common.tasksApi;

import edu.pjwstk.common.tasksApi.dto.TaskDto;
import edu.pjwstk.common.tasksApi.dto.TaskForGroupTaskRequestDto;
import edu.pjwstk.common.tasksApi.dto.TaskForGroupTaskResponseDto;

import java.util.UUID;

public interface TasksApi {
    Boolean taskExistsByTaskId(UUID taskId);

    TaskDto findTaskByTaskId(UUID taskId);

    void deleteTaskByTaskId(UUID taskId);

    TaskForGroupTaskResponseDto createTaskForGroupTask(TaskForGroupTaskRequestDto request);

}
