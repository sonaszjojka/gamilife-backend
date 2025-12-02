package edu.pjwstk.tasks.application.createtaskforgrouptask;

import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;
import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.entity.TaskCategory;
import edu.pjwstk.tasks.entity.TaskDifficulty;

import java.util.UUID;

public interface CreateTaskForGroupTaskMapper {

    Task toEntity(TaskForGroupTaskRequestDto req,
                  UUID taskId,
                  TaskCategory category,
                  TaskDifficulty difficulty
    );

    TaskForGroupTaskResponseDto toResponse(Task task);
}
