package edu.pjwstk.tasks.createhabit;

import edu.pjwstk.tasks.domain.Habit;

import java.util.UUID;

public interface CreateHabitMapper {

    Habit toEntity(CreateHabitRequest req, UUID habitId);

    CreateHabitResponse toResponse(Habit habit);
}
