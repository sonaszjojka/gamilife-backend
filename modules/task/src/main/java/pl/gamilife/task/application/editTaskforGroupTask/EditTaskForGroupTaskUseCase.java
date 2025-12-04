package pl.gamilife.task.application.editTaskforGroupTask;

import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;

import java.util.UUID;

public interface EditTaskForGroupTaskUseCase {


    public TaskForGroupTaskResponseDto execute(TaskForGroupTaskRequestDto request, UUID taskId);
}
