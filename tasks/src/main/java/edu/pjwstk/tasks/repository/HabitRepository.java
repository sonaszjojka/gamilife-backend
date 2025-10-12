package edu.pjwstk.tasks.repository;

import edu.pjwstk.tasks.entity.Habit;

import java.util.Optional;
import java.util.UUID;

public interface HabitRepository {
    Habit save(Habit habit);

    Optional<Habit> findById(UUID habitId);

    void deleteById(UUID habitId);
}
