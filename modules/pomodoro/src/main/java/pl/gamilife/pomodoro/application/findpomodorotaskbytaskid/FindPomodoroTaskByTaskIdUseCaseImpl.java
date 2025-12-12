package pl.gamilife.pomodoro.application.findpomodorotaskbytaskid;


import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.pomodoro.dto.PomodoroTaskDto;
import pl.gamilife.api.task.TasksApi;
import pl.gamilife.pomodoro.domain.repository.PomodoroTaskRepository;

import java.util.UUID;

@Service
@AllArgsConstructor
public class FindPomodoroTaskByTaskIdUseCaseImpl implements FindPomodoroTaskByTaskIdUseCase {

    private final PomodoroTaskRepository pomodoroTaskRepository;
    private final AuthApi currentUserProvider;
    private final TasksApi tasksProvider;

    @Override
    public PomodoroTaskDto execute(UUID taskId) {

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser();

        if (!currentUserDto.userId().equals(tasksProvider.findTaskByTaskId(taskId).userId())) {

            throw new UnsupportedOperationException("This task does not belong to current user");
        }

        return null;
    }
}
