package edu.pjwstk.pomodoro.repository;

import edu.pjwstk.pomodoro.domain.PomodoroTask;

import java.util.UUID;

public interface PomodoroTaskRepository {
    boolean existsByTaskId(UUID taskId);

    PomodoroTask save(PomodoroTask pomodoroTask);
    void deleteById(UUID pomodoroTaskId);
}
