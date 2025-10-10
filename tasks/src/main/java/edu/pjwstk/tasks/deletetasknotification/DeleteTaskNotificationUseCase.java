package edu.pjwstk.tasks.deletetasknotification;

import java.util.UUID;

public interface DeleteTaskNotificationUseCase {
    void execute(UUID taskId, Integer taskNotificationId);
}
