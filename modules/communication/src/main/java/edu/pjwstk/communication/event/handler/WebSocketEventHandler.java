package edu.pjwstk.communication.event.handler;

import edu.pjwstk.communication.dto.NotificationDto;
import edu.pjwstk.communication.model.NotificationRetry;
import edu.pjwstk.communication.repository.NotificationRetryRepository;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.event.EventListener;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.socket.messaging.SessionSubscribeEvent;

import java.security.Principal;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Component
@AllArgsConstructor
@Slf4j
public class WebSocketEventHandler {

    private final NotificationRetryRepository retryRepository;
    private final SimpMessagingTemplate messagingTemplate;

    @EventListener
    @Transactional
    public void onSessionSubscribeEvent(SessionSubscribeEvent event) {
        Principal user = event.getUser();
        if (user == null) {
            return;
        }

        var headers = event.getMessage().getHeaders();
        String destination = (String) headers.get("simpDestination");

        if (destination != null && (destination.endsWith("/queue/notifications") || destination.equals("/user/queue/notifications"))) {
            String userIdString = user.getName();
            log.info("User {} subscribed to notifications. Checking pending notifications...", userIdString);

            try {
                UUID userId = UUID.fromString(userIdString);
                sendPendingNotifications(userId);
            } catch (IllegalArgumentException e) {
                log.error("Invalid UUID format for user: {}", userIdString, e);
            }
        }
    }

    private void sendPendingNotifications(UUID userId) {
        List<NotificationRetry> pendingNotifications = retryRepository.findAllByUserId(userId);

        if (pendingNotifications.isEmpty()) {
            return;
        }

        log.info("Found {} pending notifications for user {}. Attempting to resend...", pendingNotifications.size(), userId);
        List<NotificationRetry> successfullySentNotifications = new ArrayList<>();
        for (NotificationRetry retry : pendingNotifications) {
            try {
                NotificationDto dto = NotificationDto.from(retry);
                messagingTemplate.convertAndSendToUser(userId.toString(), "/queue/notifications", dto);
                successfullySentNotifications.add(retry);
            } catch (Exception e) {
                log.error("Error when resending notification of id: {}", retry.getId(), e);
            }
        }

        retryRepository.deleteAll(successfullySentNotifications);
        log.info("Sent {} notifications to user {}", successfullySentNotifications.size(), userId);
    }
}
