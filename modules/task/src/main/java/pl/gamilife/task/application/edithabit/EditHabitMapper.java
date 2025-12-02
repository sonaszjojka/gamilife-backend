package pl.gamilife.task.application.edithabit;

import pl.gamilife.task.entity.Habit;

public interface EditHabitMapper {
    EditHabitResponse toResponse(Habit habit);
}
