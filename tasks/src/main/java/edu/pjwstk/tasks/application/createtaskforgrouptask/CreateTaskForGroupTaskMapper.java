package edu.pjwstk.tasks.application.createtaskforgrouptask;

import edu.pjwstk.common.tasksApi.dto.TaskForGroupTaskRequestDto;
import edu.pjwstk.common.tasksApi.dto.TaskForGroupTaskResponseDto;
import edu.pjwstk.tasks.entity.Habit;
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
