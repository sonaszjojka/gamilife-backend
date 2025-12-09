package pl.gamilife.task.application.getusertasks;

import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.pomodoro.PomodoroApi;
import pl.gamilife.api.pomodoro.dto.PomodoroTaskDto;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.task.domain.model.Habit;
import pl.gamilife.task.domain.model.filter.TaskFilter;
import pl.gamilife.task.domain.port.repository.HabitRepository;
import pl.gamilife.task.domain.port.repository.TaskRepository;
import pl.gamilife.task.infrastructure.web.response.GetUserTasksDto;

import java.util.UUID;

@Service
@AllArgsConstructor
public class GetUserTasksUseCaseImpl implements GetUserTasksUseCase {

    private final TaskRepository taskRepository;
    private final AuthApi authApi;
    private final PomodoroApi pomodoroTaskApi;
    private final HabitRepository habitRepository;

    @Override
    @Transactional
    public Page<GetUserTasksDto> execute(GetUserTasksFilterDto request) {

        CurrentUserDto userDto = authApi.getCurrentUser();
        UUID userId = userDto.userId();
        TaskFilter filter = new TaskFilter(
                userId,
                request.categoryId(),
                request.difficultyId(),
                request.isGroupTask(),
                request.isCompleted()
        );

        return taskRepository.findAll(filter, request.pageNumber(), request.pageSize())
                .map(task -> {
                    PomodoroTaskDto pomodoro = pomodoroTaskApi.findPomodoroTaskByTaskId(task.getId());
                    GetUserTasksDto.TaskHabitDto taskHabit = null;
                    Habit habit = habitRepository.findHabitByTaskId(task.getId()).orElse(null);
                    if (habit != null) {

                        taskHabit = GetUserTasksDto.TaskHabitDto.builder()
                                .habitId(habit.getId())
                                .cycleLength(habit.getCycleLength())
                                .currentStreak(habit.getCurrentStreak())
                                .longestStreak(habit.getLongestStreak())
                                .finishedAt(habit.getFinishedAt())
                                .build();
                    }
                    return new GetUserTasksDto(
                            task.getId(),
                            task.getTitle(),
                            task.getDescription(),
                            task.getDeadline(),
                            task.getCategory().getId(),
                            task.getDifficulty().getId(),
                            task.getCompletedAt(),
                            task.getCategory().getName(),
                            task.getDifficulty().getName(),
                            task.isGroupTask(),
                            task.getUserId(),
                            pomodoro,
                            taskHabit
                    );
                });
    }
}
