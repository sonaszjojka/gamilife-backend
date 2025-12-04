package pl.gamilife.gamification.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.gamification.model.Reward;

import java.util.Optional;
import java.util.UUID;

public interface RewardRepository extends JpaRepository<Reward, UUID> {
    Optional<Reward> findByStatisticTypeId(Integer statisticTypeId);
}