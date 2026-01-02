package pl.gamilife.communication.usecase.bulksendnotification;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.communication.service.NotificationService;

@Service
@AllArgsConstructor
public class BulkSendNotificationUseCaseImpl implements BulkSendNotificationUseCase {

    private final NotificationService notificationService;

    @Override
    public Void execute(BulkSendNotificationCommand cmd) {
        notificationService.bulkSendUserNotifications(cmd.userIds(), cmd.notificationDto());

        return null;
    }
}
