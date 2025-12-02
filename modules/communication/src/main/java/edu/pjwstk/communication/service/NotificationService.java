package edu.pjwstk.communication.service;

import edu.pjwstk.communication.dto.NotificationDto;

import java.util.UUID;

public interface NotificationService {
    void sendUserNotification(UUID userId, NotificationDto notification);
}
