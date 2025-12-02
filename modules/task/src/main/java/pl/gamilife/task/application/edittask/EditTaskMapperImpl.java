package pl.gamilife.task.application.edittask;

import pl.gamilife.task.entity.Task;
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
                .description(task.getDescription())
                .completedAt(task.getCompletedAt())
                .build();
    }
}
