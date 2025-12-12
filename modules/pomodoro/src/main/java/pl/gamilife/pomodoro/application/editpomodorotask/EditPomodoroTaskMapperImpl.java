package pl.gamilife.pomodoro.application.editpomodorotask;

import org.springframework.stereotype.Component;
import pl.gamilife.pomodoro.domain.PomodoroItem;

@Component
public class EditPomodoroTaskMapperImpl implements EditPomodoroTaskMapper {

    @Override
    public EditPomodoroTaskResponse toResponse(PomodoroItem pomodoroItem) {
        return EditPomodoroTaskResponse.builder()
                .pomodoroId(pomodoroItem.getId())
                .workCyclesNeeded(pomodoroItem.getCyclesRequired())
                .workCyclesCompleted(pomodoroItem.getCyclesCompleted())
                .createdAt(pomodoroItem.getCreatedAt())
                .taskId(pomodoroItem.getTaskId())
                .build();
    }
}
