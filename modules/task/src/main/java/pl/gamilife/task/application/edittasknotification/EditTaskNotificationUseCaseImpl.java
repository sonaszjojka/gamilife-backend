package pl.gamilife.task.application.edittasknotification;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.domain.model.TaskNotification;
import pl.gamilife.task.domain.port.repository.TaskNotificationRepository;

@Service
@AllArgsConstructor
public class EditTaskNotificationUseCaseImpl implements EditTaskNotificationUseCase {

    private final TaskNotificationRepository taskNotificationRepository;

    @Override
    @Transactional
    public EditTaskNotificationResponse execute(EditTaskNotificationCommand cmd) {
        TaskNotification taskNotification = taskNotificationRepository
                .findByIdAndTaskId(cmd.taskId(), cmd.taskNotificationId())
                .orElseThrow(() -> new TaskNotFoundException(
                        "Task notification with id: " + cmd.taskNotificationId() + " for task with id: " + cmd.taskId() + " not found!"
                ));

        if (!cmd.userId().equals(taskNotification.getTask().getUserId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not authorized to edit notification for another user!");
        }

        taskNotification.setSendAt(cmd.sendDate());
        taskNotificationRepository.save(taskNotification);

        return new EditTaskNotificationResponse(
                taskNotification.getId(),
                taskNotification.getSendAt(),
                taskNotification.getTask().getId()
        );
    }
}
