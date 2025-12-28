package pl.gamilife.task.application.deletetasknotification;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.domain.model.TaskNotification;
import pl.gamilife.task.domain.port.repository.TaskNotificationRepository;

@Service
@AllArgsConstructor
public class DeleteTaskNotificationUseCaseImpl implements DeleteTaskNotificationUseCase {

    private final TaskNotificationRepository taskNotificationRepository;

    @Override
    @Transactional
    public Void execute(DeleteTaskNotificationCommand cmd) {


        TaskNotification taskNotification = taskNotificationRepository
                .findByIdAndTaskId(cmd.taskId(), cmd.taskNotificationId())
                .orElseThrow(() -> new TaskNotFoundException(
                        "Task with id " + cmd.taskId() + " not found!"
                ));

        if (!cmd.userId().equals(taskNotification.getTask().getUserId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not authorized to delete notification for another user!");
        }

        taskNotificationRepository.deleteByIdAndTaskId(cmd.taskId(), cmd.taskNotificationId());

        return null;
    }
}
