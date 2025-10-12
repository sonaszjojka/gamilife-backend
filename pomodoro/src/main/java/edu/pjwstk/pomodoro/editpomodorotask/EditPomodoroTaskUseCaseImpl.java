package edu.pjwstk.pomodoro.editpomodorotask;
import edu.pjwstk.pomodoro.domain.PomodoroTask;
import edu.pjwstk.pomodoro.exception.InvalidPomodoroTaskData;
import edu.pjwstk.pomodoro.exception.PomodoroTaskNotFound;
import edu.pjwstk.pomodoro.repository.PomodoroTaskRepository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class EditPomodoroTaskUseCaseImpl implements EditPomodoroTaskUseCase {
    private final PomodoroTaskRepository pomodoroTaskRepository;
    private final EditPomodoroTaskMapper editPomodoroTaskMapper;
    public EditPomodoroTaskUseCaseImpl(PomodoroTaskRepository pomodoroTaskRepository, EditPomodoroTaskMapper editPomodoroTaskMapper) {
        this.pomodoroTaskRepository = pomodoroTaskRepository;
        this.editPomodoroTaskMapper = editPomodoroTaskMapper;
    }

    @Override
    @Transactional
    public EditPomodoroTaskResponse execute(UUID pomodoroId, EditPomodoroTaskRequest request) {
        if (request.workCyclesCompleted()>request.workCyclesNeeded()){
            throw new InvalidPomodoroTaskData("Work cycles completed cannot be larger than work cycles needed!");
        }

        PomodoroTask pomodoroTask = pomodoroTaskRepository.findByPomodoroTaskId(pomodoroId)
                .orElseThrow(() -> new PomodoroTaskNotFound("Pomodoro task with id " + pomodoroId + " not found."));
        pomodoroTask.setWorkCyclesNeeded(request.workCyclesNeeded());
        pomodoroTask.setWorkCyclesCompleted(request.workCyclesCompleted());

        return editPomodoroTaskMapper.toResponse(pomodoroTaskRepository.save(pomodoroTask));
    }
}
