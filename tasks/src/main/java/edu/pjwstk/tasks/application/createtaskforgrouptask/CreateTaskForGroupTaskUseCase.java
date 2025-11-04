package edu.pjwstk.tasks.application.createtaskforgrouptask;

import edu.pjwstk.common.tasksApi.dto.TaskForGroupTaskRequestDto;
import edu.pjwstk.common.tasksApi.dto.TaskForGroupTaskResponseDto;

public interface CreateTaskForGroupTaskUseCase {
    TaskForGroupTaskResponseDto execute(TaskForGroupTaskRequestDto request);
}
