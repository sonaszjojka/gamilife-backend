package edu.pjwstk.tasks.shared;

import pl.gamilife.api.task.dto.TaskDto;
import edu.pjwstk.tasks.entity.Task;

public interface TaskProviderMapper {
    TaskDto toResponse(Task task);
}
