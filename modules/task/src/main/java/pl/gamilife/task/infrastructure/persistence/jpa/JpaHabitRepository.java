package pl.gamilife.task.infrastructure.persistence.jpa;


import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.task.domain.model.Habit;

import java.util.Optional;
import java.util.UUID;

public interface JpaHabitRepository extends JpaRepository<Habit, UUID> {
    Optional<Habit> findHabitByTaskId(UUID id);
}
