package pl.gamilife.task.application.createtaskforgrouptask;

import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;
import pl.gamilife.task.entity.Task;
import pl.gamilife.task.entity.TaskCategory;
import pl.gamilife.task.entity.TaskDifficulty;

import java.util.UUID;

public interface CreateTaskForGroupTaskMapper {

    Task toEntity(TaskForGroupTaskRequestDto req,
                  UUID taskId,
                  TaskCategory category,
                  TaskDifficulty difficulty
    );

    TaskForGroupTaskResponseDto toResponse(Task task);
}
