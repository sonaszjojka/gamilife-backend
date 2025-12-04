package pl.gamilife.task.application.edittask;

import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public interface EditTaskUseCase {
    EditTaskResponse execute(EditTaskRequest request, UUID taskId);
}
