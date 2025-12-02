package pl.gamilife.task.application.edithabit;

import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public interface EditHabitUseCase {
    EditHabitResponse execute(EditHabitRequest request, UUID habitId, UUID taskId);
}
