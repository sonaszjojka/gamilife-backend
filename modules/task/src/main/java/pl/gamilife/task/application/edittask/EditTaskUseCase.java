package pl.gamilife.task.application.edittask;

import org.springframework.stereotype.Component;
import pl.gamilife.task.infrastructure.web.request.EditTaskRequest;
import pl.gamilife.task.infrastructure.web.response.EditTaskResponse;

import java.util.UUID;

@Component
public interface EditTaskUseCase {
    EditTaskResponse execute(EditTaskRequest request, UUID taskId);
}
