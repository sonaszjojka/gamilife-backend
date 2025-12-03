package pl.gamilife.pomodoro.usecase.editpomodorotask;

import jakarta.transaction.Transactional;
import org.springframework.stereotype.Component;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.task.TasksApi;
import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.infrastructure.core.exception.common.domain.ResourceOwnerPrivilegesRequiredException;
import pl.gamilife.pomodoro.entity.PomodoroTask;
import pl.gamilife.pomodoro.exception.domain.InvalidPomodoroTaskData;
import pl.gamilife.pomodoro.exception.domain.PomodoroTaskNotFound;
import pl.gamilife.pomodoro.repository.PomodoroTaskRepository;

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
        if (request.workCyclesCompleted() > request.workCyclesNeeded()) {
            throw new InvalidPomodoroTaskData("Work cycles completed cannot be larger than work cycles needed!");
        }

        PomodoroTask pomodoroTask = pomodoroTaskRepository.findByPomodoroTaskId(pomodoroTaskId).orElseThrow(() ->
                new PomodoroTaskNotFound("Pomodoro task with id: " + pomodoroTaskId + " does not exist"));

        TaskDto taskDto = tasksProvider.findTaskByTaskId(pomodoroTask.getTaskId());
        CurrentUserDto currentUser = currentUserProvider.getCurrentUser();
        if (!taskDto.userId().equals(currentUser.userId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not owner of the task with id: " + pomodoroTask.getTaskId());
        }

        pomodoroTask.setWorkCyclesNeeded(request.workCyclesNeeded());
        pomodoroTask.setWorkCyclesCompleted(request.workCyclesCompleted());

        return editPomodoroTaskMapper.toResponse(pomodoroTaskRepository.save(pomodoroTask));
    }
}
