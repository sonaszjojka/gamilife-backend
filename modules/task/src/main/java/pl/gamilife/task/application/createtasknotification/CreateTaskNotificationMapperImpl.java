package pl.gamilife.task.application.createtasknotification;

import org.springframework.stereotype.Component;
import pl.gamilife.task.entity.Task;
import pl.gamilife.task.entity.TaskNotification;

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
