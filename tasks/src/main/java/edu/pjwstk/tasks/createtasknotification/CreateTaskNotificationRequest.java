package edu.pjwstk.tasks.createtasknotification;

import java.time.LocalDateTime;
import java.util.UUID;

public record CreateTaskNotificationRequest(
        LocalDateTime sendDate
) {
}
