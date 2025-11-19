package edu.pjwstk.gamification.repository;

import edu.pjwstk.gamification.model.UserInventory;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface UserInventoryRepository extends JpaRepository<UserInventory, UUID> {
}