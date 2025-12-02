package pl.gamilife.task.application.edittasknotification;

import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.infrastructure.core.exception.common.domain.ResourceOwnerPrivilegesRequiredException;
import pl.gamilife.infrastructure.core.exception.common.domain.TaskNotFoundException;
import pl.gamilife.task.entity.TaskNotification;
import pl.gamilife.task.repository.TaskNotificationRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Component
public class EditTaskNotificationUseCaseImpl implements EditTaskNotificationUseCase {

    private final TaskNotificationRepository taskNotificationRepository;
    private final EditTaskNotificationMapper editHabitMapper;
    private final AuthApi currentUserProvider;

    public EditTaskNotificationUseCaseImpl(TaskNotificationRepository taskNotificationRepository,
                                           EditTaskNotificationMapper editHabitMapper, AuthApi currentUserProvider) {
        this.taskNotificationRepository = taskNotificationRepository;
        this.editHabitMapper = editHabitMapper;
        this.currentUserProvider = currentUserProvider;
    }

    @Override
    @Transactional
    public EditTaskNotificationResponse execute(EditTaskNotificationRequest request,
                                                UUID taskId, Integer taskNotificationId) {
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

        return editHabitMapper.toResponse(savedTaskNotification);
    }
}
