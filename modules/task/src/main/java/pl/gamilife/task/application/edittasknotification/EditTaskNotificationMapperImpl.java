package pl.gamilife.task.application.edittasknotification;

import org.springframework.stereotype.Component;
import pl.gamilife.task.entity.TaskNotification;

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
