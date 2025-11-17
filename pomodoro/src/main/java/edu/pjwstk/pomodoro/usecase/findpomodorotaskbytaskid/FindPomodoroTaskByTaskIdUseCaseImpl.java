package edu.pjwstk.pomodoro.usecase.findpomodorotaskbytaskid;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.pomodoroApi.dto.PomodoroTaskDto;
import edu.pjwstk.common.tasksApi.TasksApi;
import edu.pjwstk.pomodoro.repository.jpa.PomodoroTaskRepositoryJpa;
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

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser().orElseThrow();

        if (!currentUserDto.userId().equals(tasksProvider.findTaskByTaskId(taskId).userId())) {

            throw new UnsupportedOperationException("This task does not belong to current user");
        }

        return pomodoroTaskRepository.findByTaskId(taskId);
    }
}
