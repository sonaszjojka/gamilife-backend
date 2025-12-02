package pl.gamilife.task.application.createhabit;

import pl.gamilife.task.entity.Habit;
import pl.gamilife.task.entity.Task;

import java.util.UUID;

public interface CreateHabitMapper {

    Habit toEntity(CreateHabitRequest req, UUID habitId, Task habitTask);

    CreateHabitResponse toResponse(Habit habit);
}
