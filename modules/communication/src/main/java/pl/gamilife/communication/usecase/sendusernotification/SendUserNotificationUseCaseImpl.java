package pl.gamilife.communication.usecase.sendusernotification;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.communication.service.NotificationService;

@Service
@Transactional
@AllArgsConstructor
public class SendUserNotificationUseCaseImpl implements SendUserNotificationUseCase {

    private final NotificationService notificationService;

    @Override
    public Void execute(SendUserNotificationCommand cmd) {
        notificationService.sendUserNotification(cmd.userId(), cmd.notificationDto());

        return null;
    }
}
