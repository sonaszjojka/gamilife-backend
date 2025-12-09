package pl.gamilife.task.application.edittaskforgrouptask;

import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;
import pl.gamilife.task.entity.Task;

public interface EditTaskForGroupTaskMapper {

    public TaskForGroupTaskResponseDto toResponse(Task task);
}
