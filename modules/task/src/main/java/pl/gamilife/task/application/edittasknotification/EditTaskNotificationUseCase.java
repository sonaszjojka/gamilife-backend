package pl.gamilife.task.application.edittasknotification;

import org.springframework.stereotype.Component;
import pl.gamilife.shared.kernel.architecture.UseCase;
import pl.gamilife.task.infrastructure.web.request.EditTaskNotificationRequest;

@Component
public interface EditTaskNotificationUseCase extends UseCase<EditTaskNotificationCommand, EditTaskNotificationResponse> {
}
