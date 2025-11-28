package edu.pjwstk.notification.service;

import edu.pjwstk.notification.dto.NotificationDto;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Slf4j
@Service
@AllArgsConstructor
public class NotificationServiceImpl implements NotificationService {

    private final SimpMessagingTemplate messagingTemplate;

    @Override
    public void sendUserNotification(UUID userId, NotificationDto notification) {
        log.info("Message sent to {}. Content:\n{}", userId, notification);
        messagingTemplate.convertAndSendToUser(userId.toString(), "/queue/notifications", notification);
    }

}
