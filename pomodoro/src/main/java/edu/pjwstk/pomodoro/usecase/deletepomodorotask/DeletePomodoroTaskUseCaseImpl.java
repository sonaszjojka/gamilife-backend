package edu.pjwstk.pomodoro.usecase.deletepomodorotask;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.tasksApi.TasksApi;
import edu.pjwstk.common.tasksApi.dto.TaskDto;
import edu.pjwstk.pomodoro.domain.PomodoroTask;
import edu.pjwstk.pomodoro.exception.InvalidPomodoroTaskData;
import edu.pjwstk.pomodoro.exception.PomodoroTaskNotFound;
import edu.pjwstk.pomodoro.exception.UnauthorizedActionException;
import edu.pjwstk.pomodoro.repository.PomodoroTaskRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import java.util.UUID;

@Service
public class DeletePomodoroTaskUseCaseImpl implements DeletePomodoroTaskUseCase
{
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

        PomodoroTask pomodoroTask = pomodoroTaskRepository.findByPomodoroTaskId(pomodoroTaskId).orElseThrow(()->
                new PomodoroTaskNotFound("Pomodoro task with id: " + pomodoroTaskId + " does not exist"));

        TaskDto taskDto= tasksProvider.findTaskByTaskId(pomodoroTask.getTaskId());
        CurrentUserDto currentUser = currentUserProvider.getCurrentUser().orElseThrow();
        if (!taskDto.userId().equals(currentUser.userId()))
        {
            throw new UnauthorizedActionException("User is not owner of the task with id: " + pomodoroTask.getTaskId());
        }

        pomodoroTaskRepository.deleteByPomodoroTaskId(pomodoroTaskId);
    }

}
