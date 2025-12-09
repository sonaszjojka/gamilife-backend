package pl.gamilife.task.application.createtasknotification;

import pl.gamilife.task.infrastructure.web.request.CreateTaskNotificationRequest;
import pl.gamilife.task.infrastructure.web.response.CreateTaskNotificationResponse;

import java.util.UUID;

public interface CreateTaskNotificationUseCase {
    CreateTaskNotificationResponse execute(CreateTaskNotificationRequest request, UUID taskId);
}
