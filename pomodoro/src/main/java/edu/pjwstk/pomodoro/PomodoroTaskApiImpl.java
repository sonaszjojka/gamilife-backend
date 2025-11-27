package edu.pjwstk.pomodoro;

import edu.pjwstk.api.pomodoro.PomodoroApi;
import edu.pjwstk.api.pomodoro.PomodoroTaskDto.PomodoroTaskDto;
import edu.pjwstk.pomodoro.usecase.findpomodorotaskbytaskid.FindPomodoroTaskByTaskIdUseCase;
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
