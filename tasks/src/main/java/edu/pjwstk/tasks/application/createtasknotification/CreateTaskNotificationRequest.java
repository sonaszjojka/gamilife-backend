package edu.pjwstk.tasks.application.createtasknotification;

import java.time.LocalDateTime;
import java.util.UUID;

public record CreateTaskNotificationRequest(
        LocalDateTime sendDate
) {
}
