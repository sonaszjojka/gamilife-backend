package edu.pjwstk.tasks.application.createtasknotification;

import java.util.UUID;

public interface CreateTaskNotificationUseCase {
    CreateTaskNotificationResponse execute(CreateTaskNotificationRequest request, UUID taskId);
}
