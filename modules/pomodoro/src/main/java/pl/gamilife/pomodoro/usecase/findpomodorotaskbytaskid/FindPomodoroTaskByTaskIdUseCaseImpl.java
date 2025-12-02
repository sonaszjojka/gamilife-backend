package pl.gamilife.pomodoro.usecase.findpomodorotaskbytaskid;


import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.pomodoro.dto.PomodoroTaskDto;
import pl.gamilife.api.task.TasksApi;
import pl.gamilife.pomodoro.repository.jpa.PomodoroTaskRepositoryJpa;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class FindPomodoroTaskByTaskIdUseCaseImpl implements FindPomodoroTaskByTaskIdUseCase {

    private final PomodoroTaskRepositoryJpa pomodoroTaskRepository;
    private final AuthApi currentUserProvider;
    private final TasksApi tasksProvider;

    public FindPomodoroTaskByTaskIdUseCaseImpl(PomodoroTaskRepositoryJpa pomodoroTaskRepository, AuthApi currentUserProvider, TasksApi tasksProvider) {
        this.pomodoroTaskRepository = pomodoroTaskRepository;
        this.currentUserProvider = currentUserProvider;
        this.tasksProvider = tasksProvider;
    }

    @Override
    public PomodoroTaskDto execute(UUID taskId) {

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser();

        if (!currentUserDto.userId().equals(tasksProvider.findTaskByTaskId(taskId).userId())) {

            throw new UnsupportedOperationException("This task does not belong to current user");
        }

        return pomodoroTaskRepository.findByTaskId(taskId);
    }
}
