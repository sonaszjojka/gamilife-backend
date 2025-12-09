package pl.gamilife.task.application.edittasknotification;

import java.time.Instant;
import java.util.UUID;

<<<<<<<< HEAD:modules/task/src/main/java/pl/gamilife/task/application/createtasknotification/CreateTaskNotificationResult.java
public record CreateTaskNotificationResult(
========
public record EditTaskNotificationResponse(
>>>>>>>> 96cd55a0 (refactor: change half of use cases to the new structure):modules/task/src/main/java/pl/gamilife/task/application/edittasknotification/EditTaskNotificationResponse.java
        UUID id,
        Instant sendDate,
        UUID taskId
) {
}
