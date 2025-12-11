package pl.gamilife.task.infrastructure.persistence;

import org.springframework.stereotype.Repository;
import pl.gamilife.task.domain.model.Habit;
import pl.gamilife.task.domain.port.repository.HabitRepository;
import pl.gamilife.task.infrastructure.persistence.jpa.JpaHabitRepository;

import java.util.Optional;
import java.util.UUID;

@Repository
public class HabitRepositoryAdapter implements HabitRepository {

    private final JpaHabitRepository repositoryJpa;

    public HabitRepositoryAdapter(JpaHabitRepository repositoryJpa) {
        this.repositoryJpa = repositoryJpa;
    }

    @Override
    public Habit save(Habit habit) {
        return repositoryJpa.save(habit);
    }

    @Override
    public Optional<Habit> findById(UUID habitId) {
        return repositoryJpa.findById(habitId);
    }

    @Override
    public void delete(Habit habit) {
        repositoryJpa.delete(habit);
    }

}
