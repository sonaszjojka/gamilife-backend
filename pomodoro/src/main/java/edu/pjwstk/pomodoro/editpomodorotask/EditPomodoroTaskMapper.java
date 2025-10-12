package edu.pjwstk.pomodoro.editpomodorotask;
import edu.pjwstk.pomodoro.domain.PomodoroTask;

public interface EditPomodoroTaskMapper {
    EditPomodoroTaskResponse toResponse(PomodoroTask pomodoroTask);
}
