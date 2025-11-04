package edu.pjwstk.tasks.application.deletetasknotification;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.tasks.entity.TaskNotification;
import edu.pjwstk.tasks.exception.TaskNotFoundException;
import edu.pjwstk.tasks.exception.UnauthorizedTaskAccessException;
import edu.pjwstk.tasks.repository.TaskNotificationRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

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
    public void execute(UUID taskId, Integer taskNotificationId) {


        TaskNotification taskNotification = taskNotificationRepository
                .findByIdAndTaskId(taskId, taskNotificationId)
                .orElseThrow(() -> new TaskNotFoundException(
                        "Task with id " + taskId + " not found!"
                ));

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser();
        if (!currentUserDto.userId().equals(taskNotification.getTask().getUserId()) ) {
            throw new UnauthorizedTaskAccessException("User is not authorized to delete notification for another user!");
        }

        taskNotificationRepository.deleteByIdAndTaskId(taskId, taskNotificationId);
    }
}
