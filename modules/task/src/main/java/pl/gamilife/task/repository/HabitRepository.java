package pl.gamilife.task.repository;

import pl.gamilife.task.entity.Habit;

import java.util.Optional;
import java.util.UUID;

public interface HabitRepository {
    Habit save(Habit habit);

    Optional<Habit> findById(UUID habitId);

    Optional<Habit> findHabitByTaskId(UUID taskId);

    void deleteById(UUID habitId);
}
