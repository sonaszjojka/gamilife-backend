package pl.gamilife.pomodoro.repository;

import pl.gamilife.pomodoro.entity.PomodoroTask;

import java.util.Optional;
import java.util.UUID;

public interface PomodoroTaskRepository {
    boolean existsByTaskId(UUID taskId);

    PomodoroTask save(PomodoroTask pomodoroTask);
    boolean existsByPomodoroTaskId(UUID pomodoroTaskId);
    Optional <PomodoroTask> findByPomodoroTaskId(UUID pomodoroTaskId);
    void deleteByPomodoroTaskId(UUID pomodoroTaskId);
}
