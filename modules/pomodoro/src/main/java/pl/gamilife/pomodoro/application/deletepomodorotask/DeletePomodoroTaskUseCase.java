package pl.gamilife.pomodoro.application.deletepomodorotask;

import java.util.UUID;

public interface DeletePomodoroTaskUseCase {
    void execute(UUID pomodoroTaskId);
}
