package edu.pjwstk.tasks.application.edithabit;

import edu.pjwstk.tasks.entity.Habit;

public interface EditHabitMapper {
    EditHabitResponse toResponse(Habit habit);
}
