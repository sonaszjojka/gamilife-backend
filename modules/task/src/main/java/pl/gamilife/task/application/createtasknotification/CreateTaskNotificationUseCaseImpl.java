package pl.gamilife.task.application.createtasknotification;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.domain.model.Task;
import pl.gamilife.task.domain.model.TaskNotification;
import pl.gamilife.task.domain.port.repository.TaskNotificationRepository;
import pl.gamilife.task.domain.port.repository.TaskRepository;
import pl.gamilife.task.infrastructure.web.request.CreateTaskNotificationRequest;
import pl.gamilife.task.infrastructure.web.response.CreateTaskNotificationResponse;

@Service
@AllArgsConstructor
public class CreateTaskNotificationUseCaseImpl implements CreateTaskNotificationUseCase {

    private final TaskNotificationRepository taskNotificationRepository;
    private final TaskRepository taskRepository;

    @Override
    @Transactional
    public CreateTaskNotificationResult execute(CreateTaskNotificationCommand cmd) {
        Task task = taskRepository
                .findById(cmd.taskId())
                .orElseThrow(() -> new TaskNotFoundException(
                        "Task with id " + cmd.taskId() + " not found!"
                ));

        if (!cmd.userId().equals(task.getUserId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not authorized to create notification for another user!");
        }

        TaskNotification taskNotification = TaskNotification.create(cmd.taskId(), cmd.sendDate());

        taskNotification = taskNotificationRepository.save(taskNotification);

        return buildResponse(taskNotification);
    }

    private CreateTaskNotificationResult buildResponse(TaskNotification taskNotification) {
        return new CreateTaskNotificationResult(
                taskNotification.getId(),
                taskNotification.getSendDate(),
                taskNotification.getTaskId()
        );
    }
}
