package edu.pjwstk.notification.service;

import edu.pjwstk.notification.dto.NotificationDto;

import java.util.UUID;

public interface NotificationService {
    void sendUserNotification(UUID userId, NotificationDto notification);
}
