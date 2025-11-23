package edu.pjwstk.pomodoro.usecase.findpomodorotaskbytaskid;



import edu.pjwstk.api.pomodoro.PomodoroTaskDto.PomodoroTaskDto;

import java.util.UUID;

public interface FindPomodoroTaskByTaskIdUseCase {

    public PomodoroTaskDto execute(UUID taskId);
}
