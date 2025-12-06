package pl.gamilife.gamification.domain.port.repository;

import pl.gamilife.gamification.domain.model.Reward;

import java.util.Optional;

public interface RewardRepository {
    Optional<Reward> findByStatisticTypeId(Integer statisticTypeId);
}
