package pl.gamilife.pomodoro.usecase.createpomodorotask;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.task.TasksApi;
import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;
import pl.gamilife.pomodoro.entity.PomodoroTask;
import pl.gamilife.pomodoro.exception.domain.InvalidPomodoroTaskData;
import pl.gamilife.pomodoro.repository.PomodoroTaskRepository;

import java.util.UUID;

@Component
public class CreatePomodoroUseCaseImpl implements CreatePomodoroUseCase {

    private final PomodoroTaskRepository pomodoroRepository;
    private final CreatePomodoroTaskMapper createPomodoroTaskMapper;
    private final TasksApi tasksProvider;
    private final AuthApi currentUserProvider;

    public CreatePomodoroUseCaseImpl(PomodoroTaskRepository pomodoroRepository, CreatePomodoroTaskMapper createPomodoroTaskMapper, TasksApi tasksProvider, AuthApi currentUserProvider) {
        this.pomodoroRepository = pomodoroRepository;
        this.createPomodoroTaskMapper = createPomodoroTaskMapper;
        this.tasksProvider = tasksProvider;
        this.currentUserProvider = currentUserProvider;
    }

    @Override
    @Transactional
    public CreatePomodoroTaskResponse execute(UUID taskId, CreatePomodoroTaskRequest request) {
        if (request.workCyclesCompleted() > request.workCyclesNeeded()) {
            throw new InvalidPomodoroTaskData("Work cycles completed cannot be larger than work cycles needed!");
        }

        if (pomodoroRepository.existsByTaskId(taskId)) { // todo: to decide if throw 409 conflict or 400 bad request
            throw new InvalidPomodoroTaskData("Task with id:" + taskId + " has already pomodoro task!");
        }

        TaskDto taskDto = tasksProvider.findTaskByTaskId(taskId);
        CurrentUserDto currentUser = currentUserProvider.getCurrentUser();
        if (!taskDto.userId().equals(currentUser.userId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not owner of the task with id: " + taskId);
        }

        tasksProvider.findTaskByTaskId(taskId);

        PomodoroTask pomodoroTask = createPomodoroTaskMapper.toEntity(request, UUID.randomUUID(), taskId);
        PomodoroTask savedPomodoroTask = pomodoroRepository.save(pomodoroTask);
        return createPomodoroTaskMapper.toResponse(savedPomodoroTask);
    }
}
