package pl.gamilife.task.application.createtask;

import pl.gamilife.task.controllers.request.CreateTaskRequest;
import pl.gamilife.task.controllers.response.CreateTaskResponse;

public interface CreateTaskUseCase {
    CreateTaskResponse execute(CreateTaskRequest request);
}
