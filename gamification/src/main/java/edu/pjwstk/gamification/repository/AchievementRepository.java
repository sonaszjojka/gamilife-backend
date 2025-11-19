package edu.pjwstk.gamification.repository;

import edu.pjwstk.gamification.model.Achievement;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface AchievementRepository extends JpaRepository<Achievement, UUID> {
}