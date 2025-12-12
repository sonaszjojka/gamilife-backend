package pl.gamilife.pomodoro.infrastructure.api;

import org.springframework.stereotype.Service;
import pl.gamilife.api.pomodoro.PomodoroApi;
import pl.gamilife.api.pomodoro.dto.PomodoroTaskDto;
import pl.gamilife.pomodoro.application.findpomodorotaskbytaskid.FindPomodoroTaskByTaskIdUseCase;

import java.util.UUID;

@Service
public class PomodoroTaskApiImpl implements PomodoroApi {
    private final FindPomodoroTaskByTaskIdUseCase findPomodoroTaskByTaskIdUseCase;

    public PomodoroTaskApiImpl(FindPomodoroTaskByTaskIdUseCase findPomodoroTaskByTaskIdUseCase) {
        this.findPomodoroTaskByTaskIdUseCase = findPomodoroTaskByTaskIdUseCase;
    }

    @Override
    public PomodoroTaskDto findPomodoroTaskByTaskId(UUID taskId) {
        return findPomodoroTaskByTaskIdUseCase.execute(taskId);
    }
}
