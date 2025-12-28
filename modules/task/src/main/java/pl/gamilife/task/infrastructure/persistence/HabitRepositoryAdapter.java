package pl.gamilife.task.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Repository;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.task.domain.model.Habit;
import pl.gamilife.task.domain.model.filter.HabitFilter;
import pl.gamilife.task.domain.port.repository.HabitRepository;
import pl.gamilife.task.infrastructure.persistence.jpa.JpaHabitRepository;
import pl.gamilife.task.infrastructure.persistence.specification.HabitSpecificationBuilder;

import java.util.Optional;
import java.util.UUID;

@Repository
@AllArgsConstructor
public class HabitRepositoryAdapter implements HabitRepository {

    private final JpaHabitRepository repositoryJpa;
    private final HabitSpecificationBuilder habitSpecificationBuilder;

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

    @Override
    public Page<Habit> findAllHabitsFiltered(HabitFilter filter, Integer pageNumber, Integer pageSize) {
        org.springframework.data.domain.Page<Habit> habits = repositoryJpa.findAll(
                habitSpecificationBuilder.build(filter),
                PageRequest.of(pageNumber, pageSize, Sort.by(Sort.Direction.DESC, "currentDeadline"))
        );
        return new Page<>(
                habits.getContent(),
                habits.getTotalElements(),
                habits.getTotalPages(),
                habits.getNumber(),
                habits.getSize()
        );
    }

}
