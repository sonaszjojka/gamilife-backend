package edu.pjwstk.tasks.edithabit;

import edu.pjwstk.tasks.domain.Habit;

public interface EditHabitMapper {
    EditHabitResponse toResponse(Habit habit);
}
