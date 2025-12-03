package pl.gamilife.gamification.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.gamification.model.UserAchievement;

import java.util.UUID;

public interface UserAchievementRepository extends JpaRepository<UserAchievement, UUID> {
}
