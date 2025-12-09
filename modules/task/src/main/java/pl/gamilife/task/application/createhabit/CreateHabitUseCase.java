package pl.gamilife.task.application.createhabit;

import pl.gamilife.task.controllers.request.CreateHabitRequest;
import pl.gamilife.task.controllers.response.CreateHabitResponse;

import java.util.UUID;

public interface CreateHabitUseCase {
    CreateHabitResponse execute(CreateHabitRequest request, UUID taskId);
}
