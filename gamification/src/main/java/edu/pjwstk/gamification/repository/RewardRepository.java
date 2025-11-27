package edu.pjwstk.gamification.repository;

import edu.pjwstk.gamification.model.Reward;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface RewardRepository extends JpaRepository<Reward, UUID> {
    Optional<Reward> findByStatisticTypeId(Integer statisticTypeId);
}