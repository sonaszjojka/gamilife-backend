package edu.pjwstk.pomodoro.deletepomodorotask;

import java.util.UUID;

public interface DeletePomodoroTaskUseCase {
    void execute(UUID pomodoroTaskId);
}
