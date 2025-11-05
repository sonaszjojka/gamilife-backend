package edu.pjwstk.pomodoro.usecase.deletepomodorotask;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.api.tasks.TasksApi;
import edu.pjwstk.api.tasks.dto.TaskDto;
import edu.pjwstk.core.exception.common.domain.ResourceOwnerPrivilegesRequiredException;
import edu.pjwstk.pomodoro.entity.PomodoroTask;
import edu.pjwstk.pomodoro.exception.PomodoroTaskNotFound;
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
