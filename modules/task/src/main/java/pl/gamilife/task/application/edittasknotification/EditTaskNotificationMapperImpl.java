package pl.gamilife.task.application.edittasknotification;

import pl.gamilife.task.entity.TaskNotification;
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
