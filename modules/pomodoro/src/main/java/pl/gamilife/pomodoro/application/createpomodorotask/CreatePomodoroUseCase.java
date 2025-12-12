package pl.gamilife.pomodoro.application.createpomodorotask;

import java.util.UUID;

public interface CreatePomodoroUseCase {
    CreatePomodoroTaskResponse execute(UUID taskId, CreatePomodoroTaskRequest request);
}
