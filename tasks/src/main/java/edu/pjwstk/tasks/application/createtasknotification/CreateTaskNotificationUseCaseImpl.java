package edu.pjwstk.tasks.application.createtasknotification;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.entity.TaskNotification;
import edu.pjwstk.tasks.exception.TaskNotFoundException;
import edu.pjwstk.tasks.exception.UnauthorizedTaskAccessException;
import edu.pjwstk.tasks.repository.TaskNotificationRepository;
import edu.pjwstk.tasks.repository.TaskRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Component
public class CreateTaskNotificationUseCaseImpl implements CreateTaskNotificationUseCase {

    private final TaskNotificationRepository taskNotificationRepository;
    private final TaskRepository taskRepository;
    private final CreateTaskNotificationMapper createTaskNotificationMapper;
    private final AuthApi currentUserProvider;

    public CreateTaskNotificationUseCaseImpl(TaskNotificationRepository taskNotificationRepository, TaskRepository taskRepository,
                                             CreateTaskNotificationMapper createTaskNotificationMapper, AuthApi currentUserProvider) {
        this.taskNotificationRepository = taskNotificationRepository;
        this.taskRepository = taskRepository;
        this.createTaskNotificationMapper = createTaskNotificationMapper;
        this.currentUserProvider = currentUserProvider;
    }

    @Override
    @Transactional
    public CreateTaskNotificationResponse execute(CreateTaskNotificationRequest request, UUID taskId) {
        Task task = taskRepository
                .findById(taskId)
                .orElseThrow(() -> new TaskNotFoundException(
                        "Task with id " + taskId + " not found!"
                ));

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser().orElseThrow();
        if (currentUserDto.userId() != task.getUserId()) {
            throw new UnauthorizedTaskAccessException("User is not authorized to create notification for another user!");
        }

        TaskNotification taskNotification = taskNotificationRepository
                .save(createTaskNotificationMapper.toEntity(request, task));

        return createTaskNotificationMapper.toResponse(taskNotification);
    }
}
