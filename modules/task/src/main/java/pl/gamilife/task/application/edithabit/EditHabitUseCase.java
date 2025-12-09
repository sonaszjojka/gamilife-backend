package pl.gamilife.task.application.edithabit;

import org.springframework.stereotype.Component;
import pl.gamilife.task.infrastructure.web.request.EditHabitRequest;
import pl.gamilife.task.infrastructure.web.response.EditHabitResponse;

import java.util.UUID;

@Component
public interface EditHabitUseCase {
    EditHabitResponse execute(EditHabitRequest request, UUID taskId);
}
