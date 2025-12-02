package edu.pjwstk.tasks.repository;

import edu.pjwstk.tasks.entity.TaskNotification;

import java.util.Optional;
import java.util.UUID;

public interface TaskNotificationRepository {
    TaskNotification save(TaskNotification taskNotification);

    void deleteByIdAndTaskId(UUID taskId, Integer taskNotificationId);

    Optional<TaskNotification> findByIdAndTaskId(UUID taskId, Integer taskNotificationId);
}
