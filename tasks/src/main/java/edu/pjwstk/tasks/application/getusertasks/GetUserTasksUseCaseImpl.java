package edu.pjwstk.tasks.application.getusertasks;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.pomodoroApi.PomodoroTaskApi;
import edu.pjwstk.common.pomodoroApi.dto.PomodoroTaskDto;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.tasks.entity.Task;
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

    public GetUserTasksUseCaseImpl(TaskRepositoryJpa taskRepository, TasksSpecificationBuilder tasksSpecificationBuilder, AuthApi authApi, PomodoroTaskApi pomodoroTaskApi) {
        this.taskRepository = taskRepository;
        this.tasksSpecificationBuilder = tasksSpecificationBuilder;
        this.authApi = authApi;
        this.pomodoroTaskApi = pomodoroTaskApi;
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
                            pomodoro == null ? null : pomodoro.createdAt()
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