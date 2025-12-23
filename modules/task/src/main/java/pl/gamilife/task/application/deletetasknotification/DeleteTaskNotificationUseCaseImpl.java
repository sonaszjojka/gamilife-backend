package pl.gamilife.task.application.deletetasknotification;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.entity.TaskNotification;
import pl.gamilife.task.repository.TaskNotificationRepository;

import java.util.UUID;

@Component
public class DeleteTaskNotificationUseCaseImpl implements DeleteTaskNotificationUseCase {

    private final TaskNotificationRepository taskNotificationRepository;
    private final AuthApi currentUserProvider;

    public DeleteTaskNotificationUseCaseImpl(TaskNotificationRepository taskNotificationRepository, AuthApi currentUserProvider) {
        this.taskNotificationRepository = taskNotificationRepository;
        this.currentUserProvider = currentUserProvider;
    }

    @Override
    @Transactional
    public void execute(UUID taskId, UUID taskNotificationId) {


        TaskNotification taskNotification = taskNotificationRepository
                .findByIdAndTaskId(taskId, taskNotificationId)
                .orElseThrow(() -> new TaskNotFoundException(
                        "Task with id " + taskId + " not found!"
                ));

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser();
        if (!currentUserDto.userId().equals(taskNotification.getTask().getUserId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not authorized to delete notification for another user!");
        }

        taskNotificationRepository.deleteByIdAndTaskId(taskId, taskNotificationId);
    }
}
