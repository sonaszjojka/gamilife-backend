package pl.gamilife.task.application.createtasknotification;

import pl.gamilife.task.entity.Task;
import pl.gamilife.task.entity.TaskNotification;

public interface CreateTaskNotificationMapper {

    TaskNotification toEntity(CreateTaskNotificationRequest req, Task task);

    CreateTaskNotificationResponse toResponse(TaskNotification taskNotification);
}
