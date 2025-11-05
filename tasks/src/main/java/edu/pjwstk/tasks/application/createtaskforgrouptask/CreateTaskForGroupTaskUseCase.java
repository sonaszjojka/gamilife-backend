package edu.pjwstk.tasks.application.createtaskforgrouptask;

import edu.pjwstk.api.tasks.dto.TaskForGroupTaskRequestDto;
import edu.pjwstk.api.tasks.dto.TaskForGroupTaskResponseDto;

public interface CreateTaskForGroupTaskUseCase {
    TaskForGroupTaskResponseDto execute(TaskForGroupTaskRequestDto request);
}
