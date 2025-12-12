package pl.gamilife.pomodoro.domain.port.context;

import pl.gamilife.pomodoro.domain.model.projection.PomodoroHabit;
import pl.gamilife.pomodoro.domain.model.projection.PomodoroTask;

import java.time.ZoneId;
import java.util.UUID;

public interface TaskContext {
    PomodoroTask findTaskById(UUID taskId);

    PomodoroHabit findHabitById(UUID habitId, UUID userId, ZoneId zoneId);
}
