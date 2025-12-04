package pl.gamilife.communication.usecase.sendusernotification;

import jakarta.validation.ValidationException;
import pl.gamilife.communication.dto.NotificationDto;
import pl.gamilife.infrastructure.core.architecture.Command;

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
