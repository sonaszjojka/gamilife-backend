package pl.gamilife.gamification.infrastructure.persistence.jpa;

import jakarta.persistence.LockModeType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import pl.gamilife.gamification.domain.model.UserStatistic;

import java.util.Optional;
import java.util.UUID;

public interface JpaUserStatisticRepository extends JpaRepository<UserStatistic, UUID> {

    // Locking to protect from race condition
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Optional<UserStatistic> findByUserIdAndStatisticTypeId(UUID userId, Integer statisticTypeId);
}
