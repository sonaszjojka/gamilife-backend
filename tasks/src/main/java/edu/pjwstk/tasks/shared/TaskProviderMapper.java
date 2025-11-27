package edu.pjwstk.tasks.shared;

import edu.pjwstk.api.tasks.dto.TaskDto;
import edu.pjwstk.tasks.entity.Task;

public interface TaskProviderMapper {
    TaskDto toResponse(Task task);
}
