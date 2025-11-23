package edu.pjwstk.tasks.application.createhabit;

import edu.pjwstk.tasks.entity.Habit;
import edu.pjwstk.tasks.entity.Task;

import java.util.UUID;

public interface CreateHabitMapper {

    Habit toEntity(CreateHabitRequest req, UUID habitId, Task HabitTask);

    CreateHabitResponse toResponse(Habit habit);
}
