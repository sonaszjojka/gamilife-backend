package edu.pjwstk.tasks.edittask;

import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public interface EditTaskUseCase {
    EditTaskResponse execute(EditTaskRequest request, UUID taskId);
}
