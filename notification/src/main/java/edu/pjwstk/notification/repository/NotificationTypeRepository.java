package edu.pjwstk.notification.repository;

import edu.pjwstk.notification.model.NotificationType;
import org.springframework.data.jpa.repository.JpaRepository;

public interface NotificationTypeRepository extends JpaRepository<NotificationType, Integer> {
}