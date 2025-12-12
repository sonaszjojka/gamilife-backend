package pl.gamilife.task.application.getusertasks;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.task.domain.model.filter.TaskFilter;
import pl.gamilife.task.domain.port.context.UserContext;
import pl.gamilife.task.domain.port.repository.TaskRepository;
import pl.gamilife.task.infrastructure.web.response.GetUserTasksDto;

import java.time.LocalDateTime;
import java.time.ZoneId;

@Service
@Transactional(readOnly = true)
@AllArgsConstructor
public class GetUserTasksUseCaseImpl implements GetUserTasksUseCase {

    private final TaskRepository taskRepository;
    private final UserContext userContext;

    // TODO: It will only handler private tasks
    @Override
    public Page<GetUserTasksDto> execute(GetUserTasksCommand cmd) {
        TaskFilter filter = new TaskFilter(
                cmd.userId(),
                cmd.categoryId(),
                cmd.difficultyId(),
                cmd.isGroupTask(),
                cmd.isCompleted()
        );
        ZoneId zoneId = cmd.zoneId() == null ? userContext.getCurrentUserTimezone(cmd.userId()) : cmd.zoneId();

        return taskRepository.findAll(filter, cmd.pageNumber(), cmd.pageSize())
                .map(task -> {
                    // TODO: change get logic
//                    PomodoroTaskDto pomodoro = pomodoroTaskApi.findPomodoroTaskByTaskId(task.getId());
//                    GetUserTasksDto.TaskHabitDto taskHabit = null;
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
                            task.getDeadlineDate(),
                            task.getDeadlineTime(),
                            task.getCategory().getId(),
                            task.getDifficulty().getId(),
                            task.calculateCurrentStatus(LocalDateTime.now(zoneId)),
                            task.getCategory().getName(),
                            task.getDifficulty().getName(),
                            task.isGroupTask(),
                            task.getUserId()
                    );
                });
    }
}
