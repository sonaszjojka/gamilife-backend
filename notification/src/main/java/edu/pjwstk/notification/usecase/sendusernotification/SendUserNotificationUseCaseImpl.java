package edu.pjwstk.notification.usecase.sendusernotification;

import edu.pjwstk.notification.service.NotificationService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class SendUserNotificationUseCaseImpl implements SendUserNotificationUseCase {

    private final NotificationService notificationService;

    @Override
    public Void executeInternal(SendUserNotificationCommand cmd) {
        notificationService.sendUserNotification(cmd.userId(), cmd.notificationDto());

        return null;
    }
}
