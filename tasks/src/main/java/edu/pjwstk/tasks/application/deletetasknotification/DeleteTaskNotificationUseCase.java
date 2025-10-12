package edu.pjwstk.tasks.application.deletetasknotification;

import java.util.UUID;

public interface DeleteTaskNotificationUseCase {
    void execute(UUID taskId, Integer taskNotificationId);
}
