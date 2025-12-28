package pl.gamilife.api.task;

import pl.gamilife.api.task.dto.HabitDto;
import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;

import java.time.ZoneId;
import java.util.UUID;

public interface TaskApi {

    TaskDto findTaskById(UUID taskId);

    HabitDto findHabitById(UUID taskId, UUID userId, ZoneId zoneId);

    void deleteTaskByTaskId(UUID userId, UUID taskId);

    TaskForGroupTaskResponseDto createTaskForGroupTask(TaskForGroupTaskRequestDto request);

    TaskForGroupTaskResponseDto updateTaskForGroupTask(TaskForGroupTaskRequestDto request, UUID taskId);

    void completeTaskById(UUID userId, ZoneId zoneId, UUID taskId);

    void completeHabitById(UUID userId, ZoneId zoneId, UUID habitId);
}
