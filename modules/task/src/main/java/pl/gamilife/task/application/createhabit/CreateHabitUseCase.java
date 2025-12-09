package pl.gamilife.task.application.createhabit;

import pl.gamilife.task.infrastructure.web.request.CreateHabitRequest;
import pl.gamilife.task.infrastructure.web.response.CreateHabitResponse;

import java.util.UUID;

public interface CreateHabitUseCase {
    CreateHabitResponse execute(CreateHabitRequest request, UUID taskId);
}
