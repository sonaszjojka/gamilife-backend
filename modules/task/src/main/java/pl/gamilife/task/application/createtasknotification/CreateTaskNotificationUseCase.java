package pl.gamilife.task.application.createtasknotification;

import pl.gamilife.task.controllers.request.CreateTaskNotificationRequest;
import pl.gamilife.task.controllers.response.CreateTaskNotificationResponse;

import java.util.UUID;

public interface CreateTaskNotificationUseCase {
    CreateTaskNotificationResponse execute(CreateTaskNotificationRequest request, UUID taskId);
}
