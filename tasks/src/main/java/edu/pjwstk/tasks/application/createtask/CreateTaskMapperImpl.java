package edu.pjwstk.tasks.application.createtask;

import edu.pjwstk.tasks.entity.Habit;
import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.entity.TaskCategory;
import edu.pjwstk.tasks.entity.TaskDifficulty;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class CreateTaskMapperImpl implements CreateTaskMapper {

    public Task toEntity(CreateTaskRequest req,
                         UUID taskId,
                         TaskCategory category,
                         TaskDifficulty difficulty,
                         UUID userId
    ) {

        return Task.builder()
                .id(taskId)
                .title(req.title())
                .startTime(req.startTime())
                .endTime(req.endTime())
                .category(category)
                .difficulty(difficulty)
                .userId(userId)
                .description(req.description())
                .completedAt(req.completedAt())
                .isGroupTask(false)
                .build();
    }

    public CreateTaskResponse toResponse(Task task) {
        return CreateTaskResponse.builder()
                .taskId(task.getId())
                .title(task.getTitle())
                .startTime(task.getStartTime())
                .endTime(task.getEndTime())
                .categoryId(task.getCategory() != null ? task.getCategory().getId() : null)
                .difficultyId(task.getDifficulty() != null ? task.getDifficulty().getId() : null)
                .userId(task.getUserId())
                .description(task.getDescription())
                .completedAt(task.getCompletedAt())
                .build();
    }
}
