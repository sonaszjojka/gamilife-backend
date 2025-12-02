package pl.gamilife.pomodoro;

import pl.gamilife.api.pomodoro.PomodoroApi;
import pl.gamilife.api.pomodoro.dto.PomodoroTaskDto;
import pl.gamilife.pomodoro.usecase.findpomodorotaskbytaskid.FindPomodoroTaskByTaskIdUseCase;
import org.springframework.stereotype.Service;

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
