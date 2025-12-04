package pl.gamilife.communication.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.communication.model.NotificationType;

public interface NotificationTypeRepository extends JpaRepository<NotificationType, Integer> {
}