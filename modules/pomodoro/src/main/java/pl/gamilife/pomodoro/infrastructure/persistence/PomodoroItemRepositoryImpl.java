package pl.gamilife.pomodoro.infrastructure.persistence;

import jakarta.persistence.criteria.Predicate;
import lombok.AllArgsConstructor;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import pl.gamilife.pomodoro.domain.model.PomodoroItem;
import pl.gamilife.pomodoro.domain.port.repository.PomodoroItemRepository;
import pl.gamilife.pomodoro.infrastructure.persistence.jpa.PomodoroTaskRepositoryJpa;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
@AllArgsConstructor
public class PomodoroItemRepositoryImpl implements PomodoroItemRepository {

    private final PomodoroTaskRepositoryJpa pomodoroTaskRepositoryJpa;

    @Override
    public boolean existsByTaskId(UUID taskId) {
        return pomodoroTaskRepositoryJpa.existsByTaskId(taskId);
    }

    @Override
    public boolean existsByHabitId(UUID habitId) {
        return pomodoroTaskRepositoryJpa.existsByHabitId(habitId);
    }

    @Override
    public PomodoroItem save(PomodoroItem pomodoroItem) {
        return pomodoroTaskRepositoryJpa.save(pomodoroItem);
    }

    @Override
    public void deleteByPomodoroTaskId(UUID pomodoroItemId) {
        pomodoroTaskRepositoryJpa.deleteById(pomodoroItemId);
    }

    @Override
    public List<PomodoroItem> findAllByActivityIdIn(List<UUID> taskIds, List<UUID> habitIds) {
        Specification<PomodoroItem> spec = (root, query, criteriaBuilder) -> {
            Predicate taskPredicate = criteriaBuilder.and();
            Predicate habitPredicate = criteriaBuilder.and();

            if (taskIds != null && !taskIds.isEmpty()) {
                taskPredicate = root.get("taskId").in(taskIds);
            }
            if (habitIds != null && !habitIds.isEmpty()) {
                habitPredicate = root.get("habitId").in(habitIds);
            }

            return criteriaBuilder.or(taskPredicate, habitPredicate);
        };

        return pomodoroTaskRepositoryJpa.findAll(spec);
    }

    @Override
    public Optional<PomodoroItem> findById(UUID pomodoroItemId) {
        return pomodoroTaskRepositoryJpa.findById(pomodoroItemId);
    }
}
