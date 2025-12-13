package pl.gamilife.pomodoro.infrastructure.persistence.jpa;


import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import pl.gamilife.pomodoro.domain.model.PomodoroItem;

import java.util.UUID;

public interface PomodoroTaskRepositoryJpa extends JpaRepository<PomodoroItem, UUID>, JpaSpecificationExecutor<PomodoroItem> {
    boolean existsByTaskId(UUID taskId);

    boolean existsByHabitId(UUID habitId);
}
