package edu.pjwstk.communication.usecase.sendusernotification;

import edu.pjwstk.communication.service.NotificationService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class SendUserNotificationUseCaseImpl implements SendUserNotificationUseCase {

    private final NotificationService notificationService;

    @Override
    public Void execute(SendUserNotificationCommand cmd) {
        notificationService.sendUserNotification(cmd.userId(), cmd.notificationDto());

        return null;
    }
}
