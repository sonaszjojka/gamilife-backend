<<<<<<<< HEAD:modules/task/src/main/java/pl/gamilife/task/infrastructure/web/request/EditTaskRequest.java
package pl.gamilife.task.infrastructure.web.request;
========
package pl.gamilife.task.controllers.request;
>>>>>>>> ffd61dcb (chore: move requests and responses to infra):modules/task/src/main/java/pl/gamilife/task/controllers/request/EditTaskRequest.java

import jakarta.validation.constraints.FutureOrPresent;
import jakarta.validation.constraints.Size;

<<<<<<<< HEAD:modules/task/src/main/java/pl/gamilife/task/infrastructure/web/request/EditTaskRequest.java
import java.time.LocalDate;
import java.time.LocalTime;
========
import java.time.Instant;
>>>>>>>> ffd61dcb (chore: move requests and responses to infra):modules/task/src/main/java/pl/gamilife/task/controllers/request/EditTaskRequest.java

public record EditTaskRequest(
        @Size(min = 1, max = 200, message = "Title cannot exceed 200 characters")
        String title,

        Boolean removeDescription,

        @Size(min = 1, max = 500, message = "Description cannot exceed 500 characters")
        String description,

        @FutureOrPresent
        LocalDate deadlineDate,

        Boolean removeDeadlineTime,

        LocalTime deadlineTime,

        Integer categoryId,

        Integer difficultyId,

        Boolean completed
) {
}