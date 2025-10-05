package edu.pjwstk.tasks.createtasknotification;

import edu.pjwstk.tasks.domain.Habit;
import edu.pjwstk.tasks.domain.Task;
import edu.pjwstk.tasks.domain.TaskNotification;

public interface CreateTaskNotificationMapper {

    TaskNotification toEntity(CreateTaskNotificationRequest req, Task task);

    CreateTaskNotificationResponse toResponse(TaskNotification taskNotification);
}
