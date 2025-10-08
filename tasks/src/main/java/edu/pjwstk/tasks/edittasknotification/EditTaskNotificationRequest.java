package edu.pjwstk.tasks.edittasknotification;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;

import java.time.Duration;
import java.time.LocalDateTime;

public record EditTaskNotificationRequest(
        LocalDateTime sendDate
) {
}
