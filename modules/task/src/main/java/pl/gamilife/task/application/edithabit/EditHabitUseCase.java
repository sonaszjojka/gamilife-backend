package pl.gamilife.task.application.edithabit;

import org.springframework.stereotype.Component;
import pl.gamilife.shared.kernel.architecture.UseCase;
import pl.gamilife.task.infrastructure.web.request.EditHabitRequest;
import pl.gamilife.task.infrastructure.web.response.EditHabitResponse;

import java.util.UUID;

@Component
public interface EditHabitUseCase extends UseCase<EditHabitCommand, EditHabitResponse> {
}
