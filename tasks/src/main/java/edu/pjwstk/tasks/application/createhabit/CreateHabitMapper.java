package edu.pjwstk.tasks.application.createhabit;

import edu.pjwstk.tasks.entity.Habit;
import edu.pjwstk.tasks.entity.Task;

import java.util.UUID;

public interface CreateHabitMapper {

    Habit toEntity(CreateHabitRequest req, UUID habitId, Task habitTask);

    CreateHabitResponse toResponse(Habit habit);
}
