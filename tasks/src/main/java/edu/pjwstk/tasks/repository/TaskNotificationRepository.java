package edu.pjwstk.tasks.repository;

import edu.pjwstk.tasks.domain.TaskNotification;

public interface TaskNotificationRepository {
    TaskNotification save(TaskNotification taskNotification);
}
