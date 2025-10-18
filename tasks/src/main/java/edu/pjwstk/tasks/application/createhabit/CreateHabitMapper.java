package edu.pjwstk.tasks.application.createhabit;

import edu.pjwstk.tasks.entity.Habit;

import java.util.UUID;

public interface CreateHabitMapper {

    Habit toEntity(CreateHabitRequest req, UUID habitId);

    CreateHabitResponse toResponse(Habit habit);
}
