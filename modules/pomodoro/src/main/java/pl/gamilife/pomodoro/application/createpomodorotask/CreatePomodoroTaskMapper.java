package pl.gamilife.pomodoro.application.createpomodorotask;

import pl.gamilife.pomodoro.domain.PomodoroItem;

import java.util.UUID;

public interface CreatePomodoroTaskMapper {
    PomodoroItem toEntity(CreatePomodoroTaskRequest req, UUID pomodoroId, UUID taskId);

    CreatePomodoroTaskResponse toResponse(PomodoroItem pomodoroItem);
}
