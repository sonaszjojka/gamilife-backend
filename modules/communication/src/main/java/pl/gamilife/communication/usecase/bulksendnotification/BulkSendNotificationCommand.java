package pl.gamilife.communication.usecase.bulksendnotification;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.communication.dto.NotificationDto;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.Collection;
import java.util.UUID;

public record BulkSendNotificationCommand(
        @NotNull
        Collection<UUID> userIds,

        @NotNull
        NotificationDto notificationDto
) implements Command {
}
