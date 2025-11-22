package edu.pjwstk.tasks.application.getusertasks;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.pomodoroApi.PomodoroTaskApi;
import edu.pjwstk.common.pomodoroApi.dto.PomodoroTaskDto;
import edu.pjwstk.tasks.entity.Habit;
import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.repository.jpa.HabitRepositoryJpa;
import edu.pjwstk.tasks.repository.jpa.TaskRepositoryJpa;
import edu.pjwstk.tasks.util.TasksSpecificationBuilder;
import jakarta.transaction.Transactional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.data.domain.Pageable;
import java.util.UUID;

@Service
public class GetUserTasksUseCaseImpl implements GetUserTasksUseCase {
    private final TaskRepositoryJpa taskRepository;
    private final TasksSpecificationBuilder tasksSpecificationBuilder;
    private final AuthApi authApi;
    private final PomodoroTaskApi pomodoroTaskApi;
    private final HabitRepositoryJpa habitRepository;

    public GetUserTasksUseCaseImpl(TaskRepositoryJpa taskRepository, TasksSpecificationBuilder tasksSpecificationBuilder, AuthApi authApi, PomodoroTaskApi pomodoroTaskApi, HabitRepositoryJpa habitRepository) {
        this.taskRepository = taskRepository;
        this.tasksSpecificationBuilder = tasksSpecificationBuilder;
        this.authApi = authApi;
        this.pomodoroTaskApi = pomodoroTaskApi;
        this.habitRepository = habitRepository;
    }

    @Override
    @Transactional()
    public Page<GetUserTasksDto> execute(GetUserTasksFilterDto request) {

       CurrentUserDto userDto = authApi.getCurrentUser().orElseThrow();
       UUID userId = userDto.userId();





        Specification<Task> taskSpecification = tasksSpecificationBuilder.build(
                request.categoryId(),
                request.difficultyId(),
                request.isGroupTask(),
                request.isCompleted(),
                userId
        );


        Pageable pageable = createPageable(request);


        Page<GetUserTasksDto> tasks = taskRepository.findAll(taskSpecification, pageable)
                .map(task -> {
                    PomodoroTaskDto pomodoro = pomodoroTaskApi.findPomodoroTaskByTaskId(task.getId());
                    TaskHabitDto taskHabitDto= null;

                    if  (task.getHabitTask()!=null) {
                        Habit habit = habitRepository.findHabitById(task.getHabitTask().getId());

                        if (habit != null) {

                            taskHabitDto = TaskHabitDto.builder()
                                    .habitId(habit.getId())
                                    .cycleLength(habit.getCycleLength())
                                    .currentStreak(habit.getCurrentStreak())
                                    .longestStreak(habit.getLongestStreak())
                                    .isAccepted(habit.getIsAccepted())
                                    .acceptedDate(habit.getAcceptedDate())
                                    .declineMessage(habit.getDeclineMessage())
                                    .build();
                        }
                    }

                    return new GetUserTasksDto(
                            task.getId(),
                            task.getTitle(),
                            task.getDescription(),
                            task.getStartTime(),
                            task.getEndTime(),
                            task.getCategory().getId(),
                            task.getDifficulty().getId(),
                            task.getCompletedAt(),
                            task.getCategory().getTitle(),
                            task.getDifficulty().getTitle(),
                            task.getIsGroupTask(),
                            task.getUserId(),
                            pomodoro == null ? null : pomodoro.pomodoroId(),
                            pomodoro == null ? null : pomodoro.workCyclesNeeded(),
                            pomodoro == null ? null : pomodoro.workCyclesCompleted(),
                            pomodoro == null ? null : pomodoro.createdAt(),
                            taskHabitDto ==null?null: taskHabitDto.habitId(),
                            taskHabitDto ==null?null: taskHabitDto.cycleLength(),
                            taskHabitDto ==null?null: taskHabitDto.currentStreak(),
                            taskHabitDto ==null?null: taskHabitDto.longestStreak(),
                            taskHabitDto ==null?null: taskHabitDto.isAccepted(),
                            taskHabitDto ==null?null: taskHabitDto.acceptedDate(),
                            taskHabitDto ==null?null: taskHabitDto.declineMessage()

                    );
                });
        return tasks;
    }

    private Pageable createPageable(GetUserTasksFilterDto request) {

      return  PageRequest.of(
              request.pageNumber(),
              request.pageSize(),
              Sort.by(Sort.Direction.ASC,"endTime")
      );
    }
}
//ToDo : add more filters if needed + change dto for more data if needed