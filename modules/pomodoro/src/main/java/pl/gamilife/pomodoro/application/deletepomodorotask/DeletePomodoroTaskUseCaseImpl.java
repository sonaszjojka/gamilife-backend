package pl.gamilife.pomodoro.application.deletepomodorotask;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.task.TaskApi;
import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.pomodoro.domain.model.PomodoroItem;
import pl.gamilife.pomodoro.domain.exception.PomodoroItemNotFound;
import pl.gamilife.pomodoro.domain.port.repository.PomodoroItemRepository;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

import java.util.UUID;

@Service
public class DeletePomodoroTaskUseCaseImpl implements DeletePomodoroTaskUseCase {
    private final PomodoroItemRepository pomodoroItemRepository;
    private final AuthApi currentUserProvider;
    private final TaskApi tasksProvider;

    public DeletePomodoroTaskUseCaseImpl(PomodoroItemRepository pomodoroItemRepository, AuthApi currentUserProvider, TaskApi tasksProvider) {
        this.pomodoroItemRepository = pomodoroItemRepository;
        this.currentUserProvider = currentUserProvider;
        this.tasksProvider = tasksProvider;
    }

    @Override
    @Transactional
    public void execute(UUID pomodoroTaskId) {

        PomodoroItem pomodoroItem = pomodoroItemRepository.findById(pomodoroTaskId).orElseThrow(() ->
                new PomodoroItemNotFound("Pomodoro task with id: " + pomodoroTaskId + " does not exist"));

        TaskDto taskDto = tasksProvider.findTaskById(pomodoroItem.getTaskId());
        CurrentUserDto currentUser = currentUserProvider.getCurrentUser();
        if (!taskDto.userId().equals(currentUser.userId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not owner of the task with id: " + pomodoroItem.getTaskId());
        }

        pomodoroItemRepository.deleteByPomodoroTaskId(pomodoroTaskId);
    }

}
