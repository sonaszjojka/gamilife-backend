package edu.pjwstk.pomodoro.createpomodorotask;

import jakarta.validation.Valid;

import java.util.UUID;

public interface CreatePomodoroUseCase {
    CreatePomodoroTaskResponse execute(UUID taskId, CreatePomodoroTaskRequest request);
}
