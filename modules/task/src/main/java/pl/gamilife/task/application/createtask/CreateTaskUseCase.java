package pl.gamilife.task.application.createtask;

import pl.gamilife.task.infrastructure.web.request.CreateTaskRequest;
import pl.gamilife.task.infrastructure.web.response.CreateTaskResponse;

public interface CreateTaskUseCase {
    CreateTaskResponse execute(CreateTaskRequest request);
}
