package pl.gamilife.task.application.edittasknotification;

import org.springframework.stereotype.Component;
import pl.gamilife.task.controllers.request.EditTaskNotificationRequest;
import pl.gamilife.task.controllers.response.EditTaskNotificationResponse;

import java.util.UUID;

@Component
public interface EditTaskNotificationUseCase {
    EditTaskNotificationResponse execute(EditTaskNotificationRequest request,
                                         UUID taskId, UUID taskNotificationId);
}
