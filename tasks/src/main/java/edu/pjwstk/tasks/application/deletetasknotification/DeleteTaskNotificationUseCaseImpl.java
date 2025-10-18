package edu.pjwstk.tasks.application.deletetasknotification;

import edu.pjwstk.tasks.entity.TaskNotification;
import edu.pjwstk.tasks.exception.TaskNotFoundException;
import edu.pjwstk.tasks.repository.TaskNotificationRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Component
public class DeleteTaskNotificationUseCaseImpl implements DeleteTaskNotificationUseCase {

    private final TaskNotificationRepository taskNotificationRepository;

    public DeleteTaskNotificationUseCaseImpl(TaskNotificationRepository taskNotificationRepository) {
        this.taskNotificationRepository = taskNotificationRepository;
    }

    @Override
    @Transactional
    public void execute(UUID taskId, Integer taskNotificationId) {
        TaskNotification taskNotification = taskNotificationRepository
                .findByIdAndTaskId(taskId, taskNotificationId)
                .orElseThrow(() -> new TaskNotFoundException(
                        "Task with id " + taskId + " not found!"
                ));

        taskNotificationRepository.deleteByIdAndTaskId(taskId, taskNotificationId);
    }
}
