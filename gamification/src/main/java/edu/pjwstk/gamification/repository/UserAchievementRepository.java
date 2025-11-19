package edu.pjwstk.gamification.repository;

import edu.pjwstk.gamification.model.UserAchievement;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface UserAchievementRepository extends JpaRepository<UserAchievement, UUID> {
}
