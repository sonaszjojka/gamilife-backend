package pl.gamilife.task.controllers.response;

import java.time.Instant;
<<<<<<<< HEAD:modules/task/src/main/java/pl/gamilife/task/application/createhabit/CreateHabitResult.java
import java.time.LocalDate;
========
>>>>>>>> ffd61dcb (chore: move requests and responses to infra):modules/task/src/main/java/pl/gamilife/task/controllers/response/CreateHabitResponse.java
import java.util.UUID;

public record CreateHabitResult(
        UUID habitId,
        Integer cycleLength,
        LocalDate currentDeadline,
        Integer currentStreak,
        Integer longestStreak,
        Instant created_at,
        Instant updated_at
) {
}
