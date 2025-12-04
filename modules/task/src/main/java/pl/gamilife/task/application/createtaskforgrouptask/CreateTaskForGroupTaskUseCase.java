package pl.gamilife.task.application.createtaskforgrouptask;

import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;

public interface CreateTaskForGroupTaskUseCase {
    TaskForGroupTaskResponseDto execute(TaskForGroupTaskRequestDto request);
}
