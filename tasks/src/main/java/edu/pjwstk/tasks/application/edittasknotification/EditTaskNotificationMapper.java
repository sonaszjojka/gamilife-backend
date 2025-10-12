package edu.pjwstk.tasks.application.edittasknotification;

import edu.pjwstk.tasks.entity.TaskNotification;

public interface EditTaskNotificationMapper {
    EditTaskNotificationResponse toResponse(TaskNotification taskNotification);
}
