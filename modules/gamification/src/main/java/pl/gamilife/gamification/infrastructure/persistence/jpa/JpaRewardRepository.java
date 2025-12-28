package pl.gamilife.gamification.infrastructure.persistence.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.gamification.domain.model.Reward;

import java.util.Optional;
import java.util.UUID;

public interface JpaRewardRepository extends JpaRepository<Reward, UUID> {
    Optional<Reward> findByStatisticTypeId(Integer statisticTypeId);
}