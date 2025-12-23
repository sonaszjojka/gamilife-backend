package pl.gamilife.task.application.createtasknotification;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.entity.Task;
import pl.gamilife.task.entity.TaskNotification;
import pl.gamilife.task.repository.TaskNotificationRepository;
import pl.gamilife.task.repository.TaskRepository;

import java.util.UUID;

@Component
public class CreateTaskNotificationUseCaseImpl implements CreateTaskNotificationUseCase {

    private final TaskNotificationRepository taskNotificationRepository;
    private final TaskRepository taskRepository;
    private final AuthApi currentUserProvider;

    public CreateTaskNotificationUseCaseImpl(TaskNotificationRepository taskNotificationRepository, TaskRepository taskRepository, AuthApi currentUserProvider) {
        this.taskNotificationRepository = taskNotificationRepository;
        this.taskRepository = taskRepository;
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

        TaskNotification taskNotification = TaskNotification.builder()
                .taskId(taskId)
                .sendDate(request.sendDate())
                .build();
        
        taskNotification = taskNotificationRepository.save(taskNotification);

        return buildResponse(taskNotification);
    }

    private CreateTaskNotificationResponse buildResponse(TaskNotification taskNotification) {
        return new CreateTaskNotificationResponse(
                taskNotification.getId(),
                taskNotification.getSendDate(),
                taskNotification.getTaskId() // todo: might want to return whole different data
        );
    }
}
