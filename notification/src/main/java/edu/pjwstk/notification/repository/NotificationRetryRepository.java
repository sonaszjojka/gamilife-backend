package edu.pjwstk.notification.repository;

import edu.pjwstk.notification.model.NotificationRetry;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface NotificationRetryRepository extends JpaRepository<NotificationRetry, UUID> {
}