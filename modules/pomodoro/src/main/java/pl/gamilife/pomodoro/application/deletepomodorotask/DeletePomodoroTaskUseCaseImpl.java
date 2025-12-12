package pl.gamilife.pomodoro.application.deletepomodorotask;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.task.TasksApi;
import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.pomodoro.domain.PomodoroItem;
import pl.gamilife.pomodoro.domain.exception.PomodoroTaskNotFound;
import pl.gamilife.pomodoro.domain.repository.PomodoroTaskRepository;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

import java.util.UUID;

@Service
public class DeletePomodoroTaskUseCaseImpl implements DeletePomodoroTaskUseCase {
    private final PomodoroTaskRepository pomodoroTaskRepository;
    private final AuthApi currentUserProvider;
    private final TasksApi tasksProvider;

    public DeletePomodoroTaskUseCaseImpl(PomodoroTaskRepository pomodoroTaskRepository, AuthApi currentUserProvider, TasksApi tasksProvider) {
        this.pomodoroTaskRepository = pomodoroTaskRepository;
        this.currentUserProvider = currentUserProvider;
        this.tasksProvider = tasksProvider;
    }

    @Override
    @Transactional
    public void execute(UUID pomodoroTaskId) {

        PomodoroItem pomodoroItem = pomodoroTaskRepository.findById(pomodoroTaskId).orElseThrow(() ->
                new PomodoroTaskNotFound("Pomodoro task with id: " + pomodoroTaskId + " does not exist"));

        TaskDto taskDto = tasksProvider.findTaskByTaskId(pomodoroItem.getTaskId());
        CurrentUserDto currentUser = currentUserProvider.getCurrentUser();
        if (!taskDto.userId().equals(currentUser.userId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not owner of the task with id: " + pomodoroItem.getTaskId());
        }

        pomodoroTaskRepository.deleteByPomodoroTaskId(pomodoroTaskId);
    }

}
