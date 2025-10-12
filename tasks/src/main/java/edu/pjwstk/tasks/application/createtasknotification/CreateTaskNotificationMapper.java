package edu.pjwstk.tasks.application.createtasknotification;

import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.entity.TaskNotification;

public interface CreateTaskNotificationMapper {

    TaskNotification toEntity(CreateTaskNotificationRequest req, Task task);

    CreateTaskNotificationResponse toResponse(TaskNotification taskNotification);
}
