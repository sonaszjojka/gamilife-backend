package edu.pjwstk.notification.usecase.sendusernotification;

import edu.pjwstk.core.Command;
import edu.pjwstk.notification.dto.NotificationDto;
import jakarta.validation.ValidationException;

import java.util.UUID;

public record SendUserNotificationCommand(
        UUID userId,
        NotificationDto notificationDto
) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("userId cannot be null");
        }

        if (notificationDto == null) {
            throw new ValidationException("notificationDto cannot be null");
        }
    }
}
