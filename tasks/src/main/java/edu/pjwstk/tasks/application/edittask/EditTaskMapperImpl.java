package edu.pjwstk.tasks.application.edittask;

import edu.pjwstk.tasks.entity.Task;
import org.springframework.stereotype.Component;

@Component
public class EditTaskMapperImpl implements EditTaskMapper {

    @Override
    public EditTaskResponse toResponse(Task task) {
        return EditTaskResponse.builder()
                .taskId(task.getId())
                .title(task.getTitle())
                .startTime(task.getStartTime())
                .endTime(task.getEndTime())
                .categoryId(task.getCategory() != null ? task.getCategory().getId() : null)
                .difficultyId(task.getDifficulty() != null ? task.getDifficulty().getId() : null)
                .userId(task.getUserId())
                .habitTaskId(task.getHabitTask() != null ? task.getHabitTask().getId() : null)
                .previousTaskId(task.getPreviousTask() != null ? task.getPreviousTask().getId() : null)
                .description(task.getDescription())
                .completedAt(task.getCompletedAt())
                .build();
    }
}
