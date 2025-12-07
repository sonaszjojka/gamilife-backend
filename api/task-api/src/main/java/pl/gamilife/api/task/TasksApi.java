package pl.gamilife.api.task;

import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;

import java.util.UUID;

public interface TasksApi {

    TaskDto findTaskByTaskId(UUID taskId);

    void deleteTaskByTaskId(UUID taskId);

    TaskForGroupTaskResponseDto createTaskForGroupTask(TaskForGroupTaskRequestDto request);

    TaskForGroupTaskResponseDto updateTaskForGroupTask(TaskForGroupTaskRequestDto request,UUID taskId);

}
