package edu.pjwstk.pomodoro.usecase.createpomodorotask;

import edu.pjwstk.pomodoro.domain.PomodoroTask;

import java.util.UUID;

public interface CreatePomodoroTaskMapper {
    PomodoroTask toEntity(CreatePomodoroTaskRequest req, UUID pomodoroId, UUID taskId);

    CreatePomodoroTaskResponse toResponse(PomodoroTask pomodoroTask);
}
