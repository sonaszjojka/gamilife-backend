package edu.pjwstk.pomodoro.usecase.createpomodorotask;

import java.util.UUID;

public interface CreatePomodoroUseCase {
    CreatePomodoroTaskResponse execute(UUID taskId, CreatePomodoroTaskRequest request);
}
