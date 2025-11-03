package edu.pjwstk.tasks.application.createtaskforgrouptask;

import edu.pjwstk.common.tasksApi.dto.TaskForGroupTaskRequestDto;
import edu.pjwstk.common.tasksApi.dto.TaskForGroupTaskResponseDto;
import edu.pjwstk.tasks.entity.Habit;
import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.entity.TaskCategory;
import edu.pjwstk.tasks.entity.TaskDifficulty;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class CreateTaskForGroupTaskMapperImpl implements CreateTaskForGroupTaskMapper {

    public Task toEntity(TaskForGroupTaskRequestDto req,
                         UUID taskId,
                         TaskCategory category,
                         TaskDifficulty difficulty
    ) {

        return Task.builder()
                .id(taskId)
                .title(req.title())
                .startTime(req.startTime())
                .endTime(req.endTime())
                .category(category)
                .difficulty(difficulty)
                .habitTask(null)
                .previousTask(null)
                .userId(null)
                .description(req.description())
                .completedAt(req.completedAt())
                .isGroupTask(true)
                .build();
    }

    public TaskForGroupTaskResponseDto toResponse(Task task) {
        return TaskForGroupTaskResponseDto.builder()
                .taskId(task.getId())
                .title(task.getTitle())
                .startTime(task.getStartTime())
                .endTime(task.getEndTime())
                .categoryId(task.getCategory().getId())
                .difficultyId(task.getDifficulty().getId())
                .description(task.getDescription())
                .build();
    }
}
