package pl.gamilife.pomodoro.usecase.editpomodorotask;
import org.springframework.stereotype.Component;

import java.util.UUID;
@Component
public interface EditPomodoroTaskUseCase {
    EditPomodoroTaskResponse execute(UUID pomodoroId, EditPomodoroTaskRequest request);
}
