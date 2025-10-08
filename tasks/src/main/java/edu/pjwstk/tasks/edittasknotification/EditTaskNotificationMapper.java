package edu.pjwstk.tasks.edittasknotification;

import edu.pjwstk.tasks.domain.Habit;
import edu.pjwstk.tasks.domain.Task;
import edu.pjwstk.tasks.domain.TaskNotification;

public interface EditTaskNotificationMapper {
    EditTaskNotificationResponse toResponse(TaskNotification taskNotification);
}
