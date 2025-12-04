package pl.gamilife.task.shared;

import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.task.entity.Task;

public interface TaskProviderMapper {
    TaskDto toResponse(Task task);
}
