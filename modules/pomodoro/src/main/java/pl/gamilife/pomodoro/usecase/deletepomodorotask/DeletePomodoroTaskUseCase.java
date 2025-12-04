package pl.gamilife.pomodoro.usecase.deletepomodorotask;

import java.util.UUID;

public interface DeletePomodoroTaskUseCase {
    void execute(UUID pomodoroTaskId);
}
