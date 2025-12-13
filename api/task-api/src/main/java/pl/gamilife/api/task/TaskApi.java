package pl.gamilife.api.task;

import pl.gamilife.api.task.dto.*;
import pl.gamilife.shared.kernel.architecture.Page;

import java.time.ZoneId;
import java.util.UUID;

public interface TaskApi {

    TaskDto findTaskById(UUID taskId);

    HabitDto findHabitById(UUID taskId, UUID userId, ZoneId zoneId);

    void deleteTaskByTaskId(UUID userId, UUID taskId);

    TaskForGroupTaskResponseDto createTaskForGroupTask(TaskForGroupTaskRequestDto request);

    TaskForGroupTaskResponseDto updateTaskForGroupTask(TaskForGroupTaskRequestDto request,UUID taskId);

    Page<ActivityItemDto> getAllActivityItemsFiltered(ActivityItemQuery dto);

}
