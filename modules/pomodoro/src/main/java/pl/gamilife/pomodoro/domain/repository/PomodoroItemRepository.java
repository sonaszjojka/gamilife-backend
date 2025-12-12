package pl.gamilife.pomodoro.domain.repository;

import pl.gamilife.pomodoro.domain.PomodoroItem;

import java.util.Optional;
import java.util.UUID;

public interface PomodoroItemRepository {
    boolean existsByTaskId(UUID taskId);

    PomodoroItem save(PomodoroItem pomodoroItem);

    Optional<PomodoroItem> findById(UUID pomodoroItemId);

    void deleteByPomodoroTaskId(UUID pomodoroItemId);
}
