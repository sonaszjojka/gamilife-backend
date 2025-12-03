package pl.gamilife.task.shared;

import org.springframework.stereotype.Component;
import pl.gamilife.api.task.dto.TaskCategoryDto;
import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.task.entity.Task;

@Component
public class TaskProviderMapperImpl implements TaskProviderMapper {

    @Override
    public TaskDto toResponse(Task task) {
        if (task == null) {
            return null;
        }

        return TaskDto.builder()
                .id(task.getId())
                .title(task.getTitle())
                .startTime(task.getStartTime())
                .endTime(task.getEndTime())
                .category(
                        task.getCategory() != null
                                ? new TaskCategoryDto(task.getCategory().getId())
                                : null
                )
                .difficulty(
                        task.getDifficulty() != null
                                ? new TaskDto.TaskDifficultyDto(task.getDifficulty().getId())
                                : null
                )
                .userId(task.getUserId())
                .completedAt(task.getCompletedAt())
                .description(task.getDescription())
                .build();
    }
}
