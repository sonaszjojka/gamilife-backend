package edu.pjwstk.tasks.edittask;

import edu.pjwstk.tasks.domain.Task;

public interface EditTaskMapper {
    EditTaskResponse toResponse(Task task);
}
