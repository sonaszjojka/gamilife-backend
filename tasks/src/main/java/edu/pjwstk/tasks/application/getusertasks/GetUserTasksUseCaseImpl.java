package edu.pjwstk.tasks.application.getusertasks;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.api.pomodoro.PomodoroApi;
import edu.pjwstk.api.pomodoro.PomodoroTaskDto.PomodoroTaskDto;
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
    private final PomodoroApi pomodoroTaskApi;
    private final HabitRepositoryJpa habitRepository;

    public GetUserTasksUseCaseImpl(TaskRepositoryJpa taskRepository, TasksSpecificationBuilder tasksSpecificationBuilder, AuthApi authApi, PomodoroApi pomodoroTaskApi, HabitRepositoryJpa habitRepository) {
        this.taskRepository = taskRepository;
        this.tasksSpecificationBuilder = tasksSpecificationBuilder;
        this.authApi = authApi;
        this.pomodoroTaskApi = pomodoroTaskApi;
        this.habitRepository = habitRepository;
    }

    @Override
    @Transactional()
    public Page<GetUserTasksDto> execute(GetUserTasksFilterDto request) {

       CurrentUserDto userDto = authApi.getCurrentUser();
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
                    TaskHabitDto taskHabit= null;
                    Habit habit=habitRepository.findHabitByTaskId(task.getId()).orElse(null);
                    if(habit != null) {

                        taskHabit = TaskHabitDto.builder()
                                    .habitId(habit.getId())
                                    .cycleLength(habit.getCycleLength())
                                    .currentStreak(habit.getCurrentStreak())
                                    .longestStreak(habit.getLongestStreak())
                                    .acceptedDate(habit.getAcceptedDate())
                                    .build();

                        System.out.println("===============================================================================");


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
                            pomodoro,
                            taskHabit
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