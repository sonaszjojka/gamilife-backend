package pl.gamilife.task.application.createtaskforgrouptask;

import org.springframework.stereotype.Component;
import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;
import pl.gamilife.task.entity.Task;
import pl.gamilife.task.entity.TaskCategory;
import pl.gamilife.task.entity.TaskDifficulty;

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
