package edu.pjwstk.gamification.repository;

import edu.pjwstk.gamification.model.Rarity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface RarityRepository extends JpaRepository<Rarity, Integer> {
}