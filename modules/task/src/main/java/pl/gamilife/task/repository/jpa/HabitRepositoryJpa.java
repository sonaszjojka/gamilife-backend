package pl.gamilife.task.repository.jpa;


import pl.gamilife.task.entity.Habit;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface HabitRepositoryJpa extends JpaRepository<Habit, UUID> {
   Habit findHabitById(UUID id);
   Optional<Habit> findHabitByTaskId(UUID id);
}
