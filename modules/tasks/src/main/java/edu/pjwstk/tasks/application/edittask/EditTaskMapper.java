package edu.pjwstk.tasks.application.edittask;

import edu.pjwstk.tasks.entity.Task;

public interface EditTaskMapper {
    EditTaskResponse toResponse(Task task);
}
