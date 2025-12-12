package pl.gamilife.api.task;

import pl.gamilife.api.task.dto.HabitDto;
import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;

import java.util.UUID;

public interface TaskApi {

    TaskDto findTaskById(UUID taskId);

    HabitDto findHabitById(UUID taskId);

    void deleteTaskByTaskId(UUID userId, UUID taskId);

    TaskForGroupTaskResponseDto createTaskForGroupTask(TaskForGroupTaskRequestDto request);

    TaskForGroupTaskResponseDto updateTaskForGroupTask(TaskForGroupTaskRequestDto request,UUID taskId);

}
