package edu.pjwstk.tasks.application.createhabit;

import java.util.UUID;

public interface CreateHabitUseCase {
    CreateHabitResponse execute(CreateHabitRequest request, UUID taskId);
}
