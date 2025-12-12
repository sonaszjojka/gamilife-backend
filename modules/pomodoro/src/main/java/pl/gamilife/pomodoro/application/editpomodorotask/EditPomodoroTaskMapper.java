package pl.gamilife.pomodoro.application.editpomodorotask;

import pl.gamilife.pomodoro.domain.model.PomodoroItem;


public interface EditPomodoroTaskMapper {
    EditPomodoroTaskResponse toResponse(PomodoroItem pomodoroItem);
}
