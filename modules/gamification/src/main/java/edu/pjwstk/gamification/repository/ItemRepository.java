package edu.pjwstk.gamification.repository;

import edu.pjwstk.gamification.model.Item;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface ItemRepository extends JpaRepository<Item, UUID> {
}