package pl.gamilife.gamification.domain.port.repository;

import pl.gamilife.gamification.domain.model.UserStatistic;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface UserStatisticRepository {
    void saveAll(List<UserStatistic> userStatistics);

    Optional<UserStatistic> findByUserIdAndStatisticTypeId(UUID userId, Integer statisticTypeId);

    void save(UserStatistic userStatistic);
}
