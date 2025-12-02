package edu.pjwstk.tasks.application.createtasknotification;

import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.entity.TaskNotification;
import org.springframework.stereotype.Component;

@Component
public class CreateTaskNotificationMapperImpl implements CreateTaskNotificationMapper {

    @Override
    public TaskNotification toEntity(CreateTaskNotificationRequest req, Task task) {
        return TaskNotification.builder()
                .sendDate(req.sendDate())
                .task(task)
                .build();
    }

    @Override
    public CreateTaskNotificationResponse toResponse(TaskNotification taskNotification) {
        return new CreateTaskNotificationResponse(
                taskNotification.getId(),
                taskNotification.getSendDate(),
                taskNotification.getTask().getId() // todo: might want to return whole different data
        );
    }
}
