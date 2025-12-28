package pl.gamilife.gamification.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Repository;
import pl.gamilife.gamification.domain.model.Achievement;
import pl.gamilife.gamification.domain.model.projection.UserAchievementDetails;
import pl.gamilife.gamification.domain.port.repository.AchievementRepository;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaAchievementRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
@AllArgsConstructor
public class AchievementRepositoryAdapter implements AchievementRepository {

    private final JpaAchievementRepository jpaAchievementRepository;

    @Override
    public List<UserAchievementDetails> findAllAchievementDetailsByUserId(UUID userId) {
        return jpaAchievementRepository.findAllAchievementDetailsByUserId(userId);
    }

    @Override
    public Optional<Achievement> findWithItemsByStatisticTypeIdAndNotEarnedByUserId(Integer statisticTypeId, UUID userId) {
        return jpaAchievementRepository.findWithItemsByStatisticTypeIdAndNotEarnedByUserId(
                statisticTypeId,
                userId,
                PageRequest.of(0, 1)
        );
    }
}
