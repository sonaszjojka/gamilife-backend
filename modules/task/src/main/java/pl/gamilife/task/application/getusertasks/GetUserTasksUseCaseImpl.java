package pl.gamilife.task.application.getusertasks;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.pomodoro.PomodoroApi;
import pl.gamilife.api.pomodoro.dto.PomodoroTaskDto;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.task.domain.model.filter.TaskFilter;
import pl.gamilife.task.domain.port.repository.HabitRepository;
import pl.gamilife.task.domain.port.repository.TaskRepository;
import pl.gamilife.task.infrastructure.web.response.GetUserTasksDto;

@Service
@Transactional(readOnly = true)
@AllArgsConstructor
public class GetUserTasksUseCaseImpl implements GetUserTasksUseCase {

    private final TaskRepository taskRepository;
    private final PomodoroApi pomodoroTaskApi;
    private final HabitRepository habitRepository;

    @Override
    public Page<GetUserTasksDto> execute(GetUserTasksCommand cmd) {
        TaskFilter filter = new TaskFilter(
                cmd.userId(),
                cmd.categoryId(),
                cmd.difficultyId(),
                cmd.isGroupTask(),
                cmd.isCompleted()
        );

        return taskRepository.findAll(filter, cmd.pageNumber(), cmd.pageSize())
                .map(task -> {
                    PomodoroTaskDto pomodoro = pomodoroTaskApi.findPomodoroTaskByTaskId(task.getId());
                    GetUserTasksDto.TaskHabitDto taskHabit = null;
                    // TODO: change get logic
//                    Habit habit = habitRepository.findHabitByTaskId(task.getId()).orElse(null);
//                    if (habit != null) {
//
//                        taskHabit = GetUserTasksDto.TaskHabitDto.builder()
//                                .habitId(habit.getId())
//                                .cycleLength(habit.getCycleLength())
//                                .currentStreak(habit.getCurrentStreak())
//                                .longestStreak(habit.getLongestStreak())
//                                .finishedAt(habit.getFinishedAt())
//                                .build();
//                    }
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
