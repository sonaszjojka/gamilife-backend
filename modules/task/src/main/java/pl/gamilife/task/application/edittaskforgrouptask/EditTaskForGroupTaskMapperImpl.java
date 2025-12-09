package pl.gamilife.task.application.edittaskforgrouptask;

import org.springframework.stereotype.Component;
import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;
import pl.gamilife.task.entity.Task;


@Component
public class EditTaskForGroupTaskMapperImpl implements EditTaskForGroupTaskMapper {


    @Override
    public TaskForGroupTaskResponseDto toResponse(Task task) {
        return TaskForGroupTaskResponseDto.builder()
                .taskId(task.getId())
                .title(task.getTitle())
                .startTime(task.getStartTime())
                .endTime(task.getEndTime())
                .categoryId(task.getCategory() != null ? task.getCategory().getId() : null)
                .difficultyId(task.getDifficulty() != null ? task.getDifficulty().getId() : null)
                .description(task.getDescription())
                .build();
    }
}
