package edu.pjwstk.tasks.createhabit;

import edu.pjwstk.tasks.createtask.CreateTaskRequest;
import edu.pjwstk.tasks.createtask.CreateTaskResponse;
import jakarta.validation.Valid;

public interface CreateHabitUseCase {
    CreateHabitResponse execute(CreateHabitRequest request);
}
