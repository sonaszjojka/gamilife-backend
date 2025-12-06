package pl.gamilife.gamification.infrastructure.persistence.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.gamification.domain.model.UserAchievement;

import java.util.UUID;

public interface JpaUserAchievementRepository extends JpaRepository<UserAchievement, UUID> {
}
