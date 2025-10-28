package edu.pjwstk.tasks.application.edittasknotification;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.tasks.entity.TaskNotification;
import edu.pjwstk.tasks.exception.TaskNotFoundException;
import edu.pjwstk.tasks.exception.UnauthorizedTaskAccessException;
import edu.pjwstk.tasks.repository.TaskNotificationRepository;
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

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser().orElseThrow();
        if (currentUserDto.userId() != taskNotification.getTask().getUserId()) {
            throw new UnauthorizedTaskAccessException("User is not authorized to edit notification for another user!");
        }


        taskNotification.setSendDate(request.sendDate());
        TaskNotification savedTaskNotification = taskNotificationRepository.save(taskNotification);

        return editHabitMapper.toResponse(savedTaskNotification);
    }
}
