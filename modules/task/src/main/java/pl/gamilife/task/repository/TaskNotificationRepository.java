package pl.gamilife.task.repository;

import pl.gamilife.task.entity.TaskNotification;

import java.util.Optional;
import java.util.UUID;

public interface TaskNotificationRepository {
    TaskNotification save(TaskNotification taskNotification);

    void deleteByIdAndTaskId(UUID taskId, UUID taskNotificationId);

    Optional<TaskNotification> findByIdAndTaskId(UUID taskId, UUID taskNotificationId);
}
