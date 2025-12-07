package pl.gamilife.gamification.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Repository;
import pl.gamilife.gamification.domain.model.UserStatistic;
import pl.gamilife.gamification.domain.port.repository.UserStatisticRepository;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaUserStatisticRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
@AllArgsConstructor
public class UserStatisticRepositoryAdapter implements UserStatisticRepository {

    private final JpaUserStatisticRepository jpaUserStatisticRepository;

    @Override
    public void saveAll(List<UserStatistic> userStatistics) {
        jpaUserStatisticRepository.saveAll(userStatistics);
    }

    @Override
    public Optional<UserStatistic> findByUserIdAndStatisticTypeId(UUID userId, Integer statisticTypeId) {
        return jpaUserStatisticRepository.findByUserIdAndStatisticTypeId(userId, statisticTypeId);
    }

    @Override
    public void save(UserStatistic userStatistic) {
        jpaUserStatisticRepository.save(userStatistic);
    }
}
