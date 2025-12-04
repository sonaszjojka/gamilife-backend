package pl.gamilife.task.application.edittask;

import pl.gamilife.task.entity.Task;

public interface EditTaskMapper {
    EditTaskResponse toResponse(Task task);
}
