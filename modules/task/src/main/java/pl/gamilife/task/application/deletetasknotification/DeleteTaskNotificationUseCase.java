package pl.gamilife.task.application.deletetasknotification;

import java.util.UUID;

public interface DeleteTaskNotificationUseCase {
    void execute(UUID taskId, UUID taskNotificationId);
}
