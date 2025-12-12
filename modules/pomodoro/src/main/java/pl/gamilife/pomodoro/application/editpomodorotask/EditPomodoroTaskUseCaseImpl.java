package pl.gamilife.pomodoro.application.editpomodorotask;

import jakarta.transaction.Transactional;
import org.springframework.stereotype.Component;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.task.TaskApi;
import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.pomodoro.domain.model.PomodoroItem;
import pl.gamilife.pomodoro.domain.exception.InvalidPomodoroItemData;
import pl.gamilife.pomodoro.domain.exception.PomodoroItemNotFound;
import pl.gamilife.pomodoro.domain.port.repository.PomodoroItemRepository;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

import java.util.UUID;

@Component
public class EditPomodoroTaskUseCaseImpl implements EditPomodoroTaskUseCase {
    private final PomodoroItemRepository pomodoroItemRepository;
    private final EditPomodoroTaskMapper editPomodoroTaskMapper;
    private final AuthApi currentUserProvider;
    private final TaskApi tasksProvider;

    public EditPomodoroTaskUseCaseImpl(PomodoroItemRepository pomodoroItemRepository, EditPomodoroTaskMapper editPomodoroTaskMapper, AuthApi currentUserProvider, TaskApi tasksProvider) {
        this.pomodoroItemRepository = pomodoroItemRepository;
        this.editPomodoroTaskMapper = editPomodoroTaskMapper;
        this.currentUserProvider = currentUserProvider;
        this.tasksProvider = tasksProvider;
    }

    @Override
    @Transactional
    public EditPomodoroTaskResponse execute(UUID pomodoroTaskId, EditPomodoroTaskRequest request) {
        if (request.workCyclesCompleted() > request.workCyclesNeeded()) {
            throw new InvalidPomodoroItemData("Work cycles completed cannot be larger than work cycles needed!");
        }

        PomodoroItem pomodoroItem = pomodoroItemRepository.findById(pomodoroTaskId).orElseThrow(() ->
                new PomodoroItemNotFound("Pomodoro task with id: " + pomodoroTaskId + " does not exist"));

        TaskDto taskDto = tasksProvider.findTaskById(pomodoroItem.getTaskId());
        CurrentUserDto currentUser = currentUserProvider.getCurrentUser();
        if (!taskDto.userId().equals(currentUser.userId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not owner of the task with id: " + pomodoroItem.getTaskId());
        }

//        pomodoroTask.setWorkCyclesNeeded(request.cyclesRequired());
//        pomodoroTask.setWorkCyclesCompleted(request.workCyclesCompleted());

        return editPomodoroTaskMapper.toResponse(pomodoroItemRepository.save(pomodoroItem));
    }
}
