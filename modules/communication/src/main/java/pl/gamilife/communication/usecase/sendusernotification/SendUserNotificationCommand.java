package pl.gamilife.communication.usecase.sendusernotification;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.communication.dto.NotificationDto;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record SendUserNotificationCommand(
        @NotNull
        UUID userId,

        @NotNull
        NotificationDto notificationDto
) implements Command {
}
