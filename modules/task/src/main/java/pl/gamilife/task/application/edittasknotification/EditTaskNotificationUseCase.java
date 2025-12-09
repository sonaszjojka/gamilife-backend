package pl.gamilife.task.application.edittasknotification;

import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public interface EditTaskNotificationUseCase {
    EditTaskNotificationResponse execute(EditTaskNotificationRequest request,
                                         UUID taskId, UUID taskNotificationId);
}
