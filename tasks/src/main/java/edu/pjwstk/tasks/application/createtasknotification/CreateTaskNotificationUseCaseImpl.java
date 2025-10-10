package edu.pjwstk.tasks.application.createtasknotification;

import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.entity.TaskNotification;
import edu.pjwstk.tasks.exception.TaskNotFoundException;
import edu.pjwstk.tasks.repository.TaskNotificationRepository;
import edu.pjwstk.tasks.repository.TaskRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Component
public class CreateTaskNotificationUseCaseImpl implements CreateTaskNotificationUseCase {

    private final TaskNotificationRepository taskNotificationRepository;
    private final TaskRepository taskRepository;
    private final CreateTaskNotificationMapper createTaskNotificationMapper;

    public CreateTaskNotificationUseCaseImpl(TaskNotificationRepository taskNotificationRepository, TaskRepository taskRepository,
                                             CreateTaskNotificationMapper createTaskNotificationMapper) {
        this.taskNotificationRepository = taskNotificationRepository;
        this.taskRepository = taskRepository;
        this.createTaskNotificationMapper = createTaskNotificationMapper;
    }

    @Override
    @Transactional
    public CreateTaskNotificationResponse execute(CreateTaskNotificationRequest request, UUID taskId) {
        Task task = taskRepository
                .findById(taskId)
                .orElseThrow(() -> new TaskNotFoundException(
                        "Task with id " + taskId + " not found!"
                ));

        TaskNotification taskNotification = taskNotificationRepository
                .save(createTaskNotificationMapper.toEntity(request, task));

        return createTaskNotificationMapper.toResponse(taskNotification);
    }
}
