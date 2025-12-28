package pl.gamilife.pomodoro.infrastructure.persistence.jpa;


import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import pl.gamilife.pomodoro.domain.model.PomodoroItem;

import java.util.Optional;
import java.util.UUID;

public interface JpaPomodoroItemRepository extends JpaRepository<PomodoroItem, UUID>, JpaSpecificationExecutor<PomodoroItem> {
    boolean existsByTaskId(UUID taskId);

    boolean existsByHabitId(UUID habitId);

    Optional<PomodoroItem> findByTaskId(UUID taskId);

    Optional<PomodoroItem> findByHabitId(UUID taskId);
}
