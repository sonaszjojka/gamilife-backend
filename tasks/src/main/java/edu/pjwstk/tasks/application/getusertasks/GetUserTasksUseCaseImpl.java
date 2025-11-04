package edu.pjwstk.tasks.application.getusertasks;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.tasks.repository.TaskRepository;
import edu.pjwstk.tasks.repository.jpa.TaskRepositoryJpa;
import org.springframework.stereotype.Service;
import java.util.ArrayList;

import java.util.Comparator;
import java.util.List;
import java.util.UUID;

@Service
public class GetUserTasksUseCaseImpl implements GetUserTasksUseCase {
    private final TaskRepositoryJpa taskRepository;
    private final AuthApi authApi;

    public GetUserTasksUseCaseImpl(TaskRepositoryJpa taskRepository, AuthApi authApi) {
        this.taskRepository = taskRepository;
        this.authApi = authApi;
    }

    @Override
    public GetUserTasksResponse execute() {

       CurrentUserDto userDto = authApi.getCurrentUser().orElseThrow();
       UUID userId = userDto.userId();

      List <GetUserTasksDto> tasksList = new ArrayList<>(taskRepository.findByUserIdOrderByEndTimeAsc(userId)
              .stream()
              .map(task -> GetUserTasksDto.builder()
                      .taskId(task.getId())
                      .title(task.getTitle())
                      .description(task.getDescription())
                      .startTime(task.getStartTime())
                      .endTime(task.getEndTime())
                      .categoryId(task.getCategory().getId())
                      .difficultyId(task.getDifficulty().getId())
                      .completedAt(task.getCompletedAt())
                      .habitTaskId(task.getHabitTask().getId())
                      .previousTaskId(task.getPreviousTask() != null ? task.getPreviousTask().getId() : null)
                      .description(task.getDescription())
                      .build())
              .toList());



        return GetUserTasksResponse.builder()
                .tasks(tasksList)
                .build();
    }
}
