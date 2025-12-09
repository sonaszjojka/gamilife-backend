package pl.gamilife.task.domain.port.repository;

import pl.gamilife.task.domain.model.TaskNotification;

import java.util.Optional;
import java.util.UUID;

public interface TaskNotificationRepository {
    TaskNotification save(TaskNotification taskNotification);

    void deleteByIdAndTaskId(UUID taskId, UUID taskNotificationId);

    Optional<TaskNotification> findByIdAndTaskId(UUID taskId, UUID taskNotificationId);
}
