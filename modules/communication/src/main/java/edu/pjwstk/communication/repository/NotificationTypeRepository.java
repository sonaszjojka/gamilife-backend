package edu.pjwstk.communication.repository;

import edu.pjwstk.communication.model.NotificationType;
import org.springframework.data.jpa.repository.JpaRepository;

public interface NotificationTypeRepository extends JpaRepository<NotificationType, Integer> {
}