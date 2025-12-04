package pl.gamilife.task.application.editTaskforGroupTask;

import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;
import pl.gamilife.task.entity.Task;

public interface EditTaskForGroupTaskMapper {

    public TaskForGroupTaskResponseDto toResponse(Task task);
}
