package pl.gamilife.gamification.repository;

import pl.gamilife.gamification.model.UserAchievement;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface UserAchievementRepository extends JpaRepository<UserAchievement, UUID> {
}
