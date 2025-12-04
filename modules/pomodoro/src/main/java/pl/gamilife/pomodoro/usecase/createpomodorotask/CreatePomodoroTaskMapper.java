package pl.gamilife.pomodoro.usecase.createpomodorotask;

import pl.gamilife.pomodoro.entity.PomodoroTask;

import java.util.UUID;

public interface CreatePomodoroTaskMapper {
    PomodoroTask toEntity(CreatePomodoroTaskRequest req, UUID pomodoroId, UUID taskId);

    CreatePomodoroTaskResponse toResponse(PomodoroTask pomodoroTask);
}
