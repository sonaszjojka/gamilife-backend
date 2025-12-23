package pl.gamilife.task.application.edittasknotification;

import org.springframework.stereotype.Component;
import pl.gamilife.shared.kernel.architecture.UseCase;

@Component
public interface EditTaskNotificationUseCase extends UseCase<EditTaskNotificationCommand, EditTaskNotificationResponse> {
}
