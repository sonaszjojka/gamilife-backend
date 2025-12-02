package pl.gamilife.communication.service;

import pl.gamilife.communication.dto.NotificationDto;
import pl.gamilife.communication.model.NotificationRetry;
import pl.gamilife.communication.repository.NotificationRetryRepository;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.messaging.simp.user.SimpUser;
import org.springframework.messaging.simp.user.SimpUserRegistry;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Slf4j
@Service
@AllArgsConstructor
public class NotificationServiceImpl implements NotificationService {

    private final SimpMessagingTemplate messagingTemplate;
    private final SimpUserRegistry userRegistry;
    private final NotificationRetryRepository notificationRetryRepository;

    @Override
    @Transactional
    public void sendUserNotification(UUID userId, NotificationDto notification) {
        if (isUserSubscribingToNotifications(userId)) {
            log.info("User {} is online. Sending notification.", userId);
            try {
                messagingTemplate.convertAndSendToUser(userId.toString(), "/queue/notifications", notification);
                return;
            } catch (Exception e) {
                log.warn("Failed to send notification to user {}", userId, e);
            }
        }

        log.info("Saving notification retry for user {}.", userId);
        saveNotificationToRetry(userId, notification);
    }

    private boolean isUserSubscribingToNotifications(UUID userId) {
        SimpUser user = userRegistry.getUser(userId.toString());

        if (user == null || !user.hasSessions()) {
            return false;
        }

        boolean subscribed = user.getSessions().stream()
                .flatMap(s -> s.getSubscriptions().stream())
                .anyMatch(sub -> sub.getDestination().endsWith("/queue/notifications"));

        if (!subscribed) {
            log.debug("User {} is online, but not subscribed to /queue/notifications.", userId);
        }
        return subscribed;
    }

    private void saveNotificationToRetry(UUID userId, NotificationDto dto) {
        NotificationRetry retry = NotificationRetry.builder()
                .id(dto.getId() != null ? dto.getId() : UUID.randomUUID())
                .userId(userId)
                .title(dto.getTitle())
                .message(dto.getMessage())
                .originalTimestamp(dto.getTimestamp())
                .data(dto.getData())
                .notificationTypeId(dto.getNotificationType().getId())
                .build();

        notificationRetryRepository.save(retry);
    }
}
