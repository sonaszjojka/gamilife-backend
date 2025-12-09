<<<<<<<< HEAD:modules/task/src/main/java/pl/gamilife/task/application/createtask/CreateTaskCommand.java
package pl.gamilife.task.application.createtask;
========
package pl.gamilife.task.infrastructure.web.request;
>>>>>>>> d3529bc6 (refactor: start refactor of task module):modules/task/src/main/java/pl/gamilife/task/infrastructure/web/request/CreateTaskRequest.java

import jakarta.validation.constraints.FutureOrPresent;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.UUID;

public record CreateTaskCommand(
        @NotNull
        UUID userId,

        ZoneId zoneId,

        @NotBlank(message = "Title cannot be blank")
        @Size(max = 200, message = "Title cannot exceed 200 characters")
        String title,

        @Size(min = 1, max = 200, message = "Description cannot exceed 500 characters")
        String description,

        @FutureOrPresent
        @NotNull(message = "Deadline cannot be null")
        LocalDate deadlineDate,

        LocalTime deadlineTime,

        @NotNull(message = "Category Id cannot be null")
        Integer categoryId,

        @NotNull(message = "Difficulty Id cannot be null")
        Integer difficultyId
) implements Command {
}
