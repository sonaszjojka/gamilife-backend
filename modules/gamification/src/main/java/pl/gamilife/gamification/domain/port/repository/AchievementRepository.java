package pl.gamilife.gamification.domain.port.repository;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.gamification.domain.model.Achievement;
import pl.gamilife.gamification.domain.model.projection.UserAchievementDetails;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface AchievementRepository {
    List<UserAchievementDetails> findAllAchievementDetailsByUserId(UUID userId);

    Optional<Achievement> findWithItemsByStatisticTypeIdAndNotEarnedByUserId(@NotNull Integer statisticTypeId, @NotNull UUID userId);
}
