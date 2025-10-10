package edu.pjwstk.tasks.application.edittasknotification;

import edu.pjwstk.tasks.entity.TaskNotification;
import edu.pjwstk.tasks.exception.TaskNotFoundException;
import edu.pjwstk.tasks.repository.TaskNotificationRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Component
public class EditTaskNotificationUseCaseImpl implements EditTaskNotificationUseCase {

    private final TaskNotificationRepository taskNotificationRepository;
    private final EditTaskNotificationMapper editHabitMapper;

    public EditTaskNotificationUseCaseImpl(TaskNotificationRepository taskNotificationRepository,
                                           EditTaskNotificationMapper editHabitMapper) {
        this.taskNotificationRepository = taskNotificationRepository;
        this.editHabitMapper = editHabitMapper;
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

        taskNotification.setSendDate(request.sendDate());
        TaskNotification savedTaskNotification = taskNotificationRepository.save(taskNotification);

        return editHabitMapper.toResponse(savedTaskNotification);
    }
}
