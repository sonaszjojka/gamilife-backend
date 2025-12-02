package edu.pjwstk.pomodoro.usecase.deletepomodorotask;

import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.task.TasksApi;
import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.infrastructure.core.exception.common.domain.ResourceOwnerPrivilegesRequiredException;
import edu.pjwstk.pomodoro.entity.PomodoroTask;
import edu.pjwstk.pomodoro.exception.domain.PomodoroTaskNotFound;
import edu.pjwstk.pomodoro.repository.PomodoroTaskRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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

        PomodoroTask pomodoroTask = pomodoroTaskRepository.findByPomodoroTaskId(pomodoroTaskId).orElseThrow(() ->
                new PomodoroTaskNotFound("Pomodoro task with id: " + pomodoroTaskId + " does not exist"));

        TaskDto taskDto = tasksProvider.findTaskByTaskId(pomodoroTask.getTaskId());
        CurrentUserDto currentUser = currentUserProvider.getCurrentUser();
        if (!taskDto.userId().equals(currentUser.userId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not owner of the task with id: " + pomodoroTask.getTaskId());
        }

        pomodoroTaskRepository.deleteByPomodoroTaskId(pomodoroTaskId);
    }

}
