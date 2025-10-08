package edu.pjwstk.tasks.edittasknotification;

import edu.pjwstk.tasks.domain.Habit;
import edu.pjwstk.tasks.domain.Task;
import edu.pjwstk.tasks.domain.TaskNotification;
import org.springframework.stereotype.Component;

@Component
public class EditTaskNotificationMapperImpl implements EditTaskNotificationMapper {

    public EditTaskNotificationResponse toResponse(TaskNotification taskNotification) {
        return new EditTaskNotificationResponse(
                taskNotification.getId(),
                taskNotification.getSendDate(),
                taskNotification.getTask().getId()
        );
    }

}
