package pl.gamilife.pomodoro.infrastructure.external;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import pl.gamilife.api.task.TaskApi;
import pl.gamilife.api.task.dto.HabitDto;
import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.pomodoro.domain.model.projection.PomodoroHabit;
import pl.gamilife.pomodoro.domain.model.projection.PomodoroTask;
import pl.gamilife.pomodoro.domain.port.context.TaskContext;

import java.time.ZoneId;
import java.util.UUID;

@Component
@AllArgsConstructor
public class PomodoroTaskContextAdapter implements TaskContext {

    private final TaskApi taskApi;

    @Override
    public PomodoroTask findTaskById(UUID taskId) {
        TaskDto taskDto = taskApi.findTaskById(taskId);

        return new PomodoroTask(taskDto.userId());
    }

    @Override
    public PomodoroHabit findHabitById(UUID habitId, UUID userId, ZoneId zoneId) {
        HabitDto habitDto = taskApi.findHabitById(habitId, userId, zoneId);
        return new PomodoroHabit(habitDto.userId(), habitDto.canBeWorkedOn());
    }
}
