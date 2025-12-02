package pl.gamilife.pomodoro.usecase.editpomodorotask;

import pl.gamilife.pomodoro.entity.PomodoroTask;


public interface EditPomodoroTaskMapper {
    EditPomodoroTaskResponse toResponse(PomodoroTask pomodoroTask);
}
