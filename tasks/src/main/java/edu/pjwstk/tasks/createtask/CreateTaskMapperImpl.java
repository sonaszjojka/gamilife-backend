package edu.pjwstk.tasks.createtask;

import edu.pjwstk.tasks.domain.Habit;
import edu.pjwstk.tasks.domain.Task;
import edu.pjwstk.tasks.domain.TaskCategory;
import edu.pjwstk.tasks.domain.TaskDifficulty;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class CreateTaskMapperImpl implements CreateTaskMapper {

    public Task toEntity(CreateTaskRequest req,
                         UUID taskId,
                         TaskCategory category,
                         TaskDifficulty difficulty,
                         Habit habitTask,
                         Task previousTask) {

        return Task.builder()
                .id(taskId)
                .title(req.title())
                .startTime(req.startTime())
                .endTime(req.endTime())
                .category(category)
                .difficulty(difficulty)
                .userId(req.userId())
                .habitTask(habitTask)
                .previousTask(previousTask)
                .description(req.description())
                .completedAt(req.completedAt())
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
                .taskHabitId(task.getHabitTask() != null ? task.getHabitTask().getId() : null)
                .previousTaskId(task.getPreviousTask() != null ? task.getPreviousTask().getId() : null)
                .description(task.getDescription())
                .completedAt(task.getCompletedAt())
                .build();
    }
}
