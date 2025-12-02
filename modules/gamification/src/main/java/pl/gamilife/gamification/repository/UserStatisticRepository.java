package pl.gamilife.gamification.repository;

import pl.gamilife.gamification.model.UserStatistic;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface UserStatisticRepository extends JpaRepository<UserStatistic, UUID> {

    Optional<UserStatistic> findByUserIdAndStatisticTypeId(UUID userId, Integer statisticTypeId);
}
