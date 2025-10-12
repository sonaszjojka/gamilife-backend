package edu.pjwstk.tasks.application.taskexistsbyid;

import edu.pjwstk.tasks.application.edittasknotification.EditTaskNotificationMapper;
import edu.pjwstk.tasks.application.edittasknotification.EditTaskNotificationRequest;
import edu.pjwstk.tasks.application.edittasknotification.EditTaskNotificationResponse;
import edu.pjwstk.tasks.entity.TaskNotification;
import edu.pjwstk.tasks.exception.TaskNotFoundException;
import edu.pjwstk.tasks.repository.TaskNotificationRepository;
import edu.pjwstk.tasks.repository.TaskRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Component
public class ExistsByTaskIdUseCaseImpl implements ExistsByTaskIdUseCase {

    private final TaskRepository taskRepository;

    public ExistsByTaskIdUseCaseImpl(TaskRepository taskRepository) {
        this.taskRepository = taskRepository;
    }

    @Override
    public Boolean execute(UUID taskId) {
        return taskRepository.existsById(taskId);
    }
}
