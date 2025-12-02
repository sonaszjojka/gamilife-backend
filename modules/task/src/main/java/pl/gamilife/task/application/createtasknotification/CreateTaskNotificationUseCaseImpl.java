package pl.gamilife.task.application.createtasknotification;

import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.infrastructure.core.exception.common.domain.ResourceOwnerPrivilegesRequiredException;
import pl.gamilife.infrastructure.core.exception.common.domain.TaskNotFoundException;
import pl.gamilife.task.entity.Task;
import pl.gamilife.task.entity.TaskNotification;
import pl.gamilife.task.repository.TaskNotificationRepository;
import pl.gamilife.task.repository.TaskRepository;
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

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser();
        if (!currentUserDto.userId().equals(task.getUserId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not authorized to create notification for another user!");
        }

        TaskNotification taskNotification = taskNotificationRepository
                .save(createTaskNotificationMapper.toEntity(request, task));

        return createTaskNotificationMapper.toResponse(taskNotification);
    }
}
