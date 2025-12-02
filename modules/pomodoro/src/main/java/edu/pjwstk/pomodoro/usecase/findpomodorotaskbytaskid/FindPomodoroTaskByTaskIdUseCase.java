package edu.pjwstk.pomodoro.usecase.findpomodorotaskbytaskid;



import pl.gamilife.api.pomodoro.dto.PomodoroTaskDto;

import java.util.UUID;

public interface FindPomodoroTaskByTaskIdUseCase {

    public PomodoroTaskDto execute(UUID taskId);
}
