package edu.pjwstk.pomodoro.usecase.editpomodorotask;
import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.tasksApi.TasksApi;
import edu.pjwstk.common.tasksApi.dto.TaskDto;
import edu.pjwstk.pomodoro.domain.PomodoroTask;
import edu.pjwstk.pomodoro.exception.InvalidPomodoroTaskData;
import edu.pjwstk.pomodoro.exception.PomodoroTaskNotFound;
import edu.pjwstk.pomodoro.exception.UnauthorizedActionException;
import edu.pjwstk.pomodoro.repository.PomodoroTaskRepository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class EditPomodoroTaskUseCaseImpl implements EditPomodoroTaskUseCase {
    private final PomodoroTaskRepository pomodoroTaskRepository;
    private final EditPomodoroTaskMapper editPomodoroTaskMapper;
    private final AuthApi currentUserProvider;
    private final TasksApi tasksProvider;
    public EditPomodoroTaskUseCaseImpl(PomodoroTaskRepository pomodoroTaskRepository, EditPomodoroTaskMapper editPomodoroTaskMapper, AuthApi currentUserProvider, TasksApi tasksProvider) {
        this.pomodoroTaskRepository = pomodoroTaskRepository;
        this.editPomodoroTaskMapper = editPomodoroTaskMapper;
        this.currentUserProvider = currentUserProvider;
        this.tasksProvider = tasksProvider;
    }

    @Override
    @Transactional
    public EditPomodoroTaskResponse execute(UUID pomodoroTaskId, EditPomodoroTaskRequest request) {
        if (request.workCyclesCompleted()>request.workCyclesNeeded()){
            throw new InvalidPomodoroTaskData("Work cycles completed cannot be larger than work cycles needed!");
        }

        PomodoroTask pomodoroTask = pomodoroTaskRepository.findByPomodoroTaskId(pomodoroTaskId).orElseThrow(()->
                new PomodoroTaskNotFound("Pomodoro task with id: " + pomodoroTaskId + " does not exist"));

        TaskDto taskDto= tasksProvider.findTaskByTaskId(pomodoroTask.getTaskId());
        CurrentUserDto currentUser = currentUserProvider.getCurrentUser().orElseThrow();
        if (!taskDto.userId().equals(currentUser.userId()))
        {
            throw new UnauthorizedActionException("User is not owner of the task with id: " + pomodoroTask.getTaskId());
        }

        pomodoroTask.setWorkCyclesNeeded(request.workCyclesNeeded());
        pomodoroTask.setWorkCyclesCompleted(request.workCyclesCompleted());

        return editPomodoroTaskMapper.toResponse(pomodoroTaskRepository.save(pomodoroTask));
    }
}
