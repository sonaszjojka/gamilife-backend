package pl.gamilife.task.application.edittasknotification;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.infrastructure.web.request.EditTaskNotificationRequest;
import pl.gamilife.task.infrastructure.web.response.EditTaskNotificationResponse;
import pl.gamilife.task.domain.model.TaskNotification;
import pl.gamilife.task.domain.port.repository.TaskNotificationRepository;

import java.util.UUID;

@Component
public class EditTaskNotificationUseCaseImpl implements EditTaskNotificationUseCase {

    private final TaskNotificationRepository taskNotificationRepository;
    private final AuthApi currentUserProvider;

    public EditTaskNotificationUseCaseImpl(TaskNotificationRepository taskNotificationRepository, AuthApi currentUserProvider) {
        this.taskNotificationRepository = taskNotificationRepository;
        this.currentUserProvider = currentUserProvider;
    }

    @Override
    @Transactional
    public EditTaskNotificationResponse execute(EditTaskNotificationRequest request,
                                                UUID taskId, UUID taskNotificationId) {
        TaskNotification taskNotification = taskNotificationRepository
                .findByIdAndTaskId(taskId, taskNotificationId)
                .orElseThrow(() -> new TaskNotFoundException(
                        "Task notification with id: " + taskNotificationId + " for task with id: " + taskId + " not found!"
                ));

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser();
        if (!currentUserDto.userId().equals(taskNotification.getTask().getUserId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not authorized to edit notification for another user!");
        }


        taskNotification.setSendDate(request.sendDate());
        TaskNotification savedTaskNotification = taskNotificationRepository.save(taskNotification);

        return new EditTaskNotificationResponse(
                taskNotification.getId(),
                taskNotification.getSendDate(),
                taskNotification.getTask().getId()
        );
    }
}
