package pl.gamilife.task.application.edittasknotification;

import pl.gamilife.task.entity.TaskNotification;

public interface EditTaskNotificationMapper {
    EditTaskNotificationResponse toResponse(TaskNotification taskNotification);
}
