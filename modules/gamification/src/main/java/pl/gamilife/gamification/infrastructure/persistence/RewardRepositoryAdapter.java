package pl.gamilife.gamification.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Repository;
import pl.gamilife.gamification.domain.model.Reward;
import pl.gamilife.gamification.domain.port.repository.RewardRepository;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaRewardRepository;

import java.util.Optional;

@Repository
@AllArgsConstructor
public class RewardRepositoryAdapter implements RewardRepository {

    private final JpaRewardRepository jpaRewardRepository;

    @Override
    public Optional<Reward> findByStatisticTypeId(Integer statisticTypeId) {
        return jpaRewardRepository.findByStatisticTypeId(statisticTypeId);
    }
}
