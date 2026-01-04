package pl.gamilife.gamification.infrastructure.persistence.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.gamification.domain.model.UserStatistic;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface JpaUserStatisticRepository extends JpaRepository<UserStatistic, UUID> {
    Optional<UserStatistic> findByUserIdAndStatisticTypeId(UUID userId, Integer statisticTypeId);
    List<UserStatistic> findByUserId(UUID userId);
}
