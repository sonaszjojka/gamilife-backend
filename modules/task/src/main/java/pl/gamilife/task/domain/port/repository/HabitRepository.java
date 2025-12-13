package pl.gamilife.task.domain.port.repository;

import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.task.domain.model.Habit;
import pl.gamilife.task.domain.model.filter.HabitFilter;

import java.util.Optional;
import java.util.UUID;

public interface HabitRepository {
    Habit save(Habit habit);

    Optional<Habit> findById(UUID habitId);

    void delete(Habit habit);

    Page<Habit> findAllHabitsFiltered(HabitFilter filter, Integer pageNumber, Integer pageSize);
}
