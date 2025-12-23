package pl.gamilife.pomodoro.domain.port.repository;

import pl.gamilife.pomodoro.domain.model.PomodoroItem;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface PomodoroItemRepository {
    boolean existsByTaskId(UUID taskId);

    boolean existsByHabitId(UUID habitId);

    PomodoroItem save(PomodoroItem pomodoroItem);

    Optional<PomodoroItem> findById(UUID pomodoroItemId);

    void deleteByPomodoroTaskId(UUID pomodoroItemId);

    List<PomodoroItem> findAllByActivityIdIn(List<UUID> taskIds, List<UUID> habitIds);
}
