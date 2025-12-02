package pl.gamilife.communication.repository;

import pl.gamilife.communication.model.NotificationRetry;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface NotificationRetryRepository extends JpaRepository<NotificationRetry, UUID> {
    List<NotificationRetry> findAllByUserId(UUID userId);
}