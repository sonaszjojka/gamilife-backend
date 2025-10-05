package edu.pjwstk.tasks.repository;

import edu.pjwstk.tasks.domain.Habit;

public interface HabitRepository {
    Habit save(Habit habit);
}
