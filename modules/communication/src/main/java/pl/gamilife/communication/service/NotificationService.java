package pl.gamilife.communication.service;

import pl.gamilife.communication.dto.NotificationDto;

import java.util.UUID;

public interface NotificationService {
    void sendUserNotification(UUID userId, NotificationDto notification);
}
