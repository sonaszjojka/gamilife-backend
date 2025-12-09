package pl.gamilife.task.domain.port.repository;

import pl.gamilife.task.domain.model.Habit;

import java.util.Optional;
import java.util.UUID;

public interface HabitRepository {
    Habit save(Habit habit);

    Optional<Habit> findById(UUID habitId);

    Optional<Habit> findHabitByTaskId(UUID taskId);

    void delete(Habit habit);
}
