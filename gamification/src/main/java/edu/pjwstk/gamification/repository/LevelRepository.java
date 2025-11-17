package edu.pjwstk.gamification.repository;

import edu.pjwstk.gamification.model.Level;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface LevelRepository extends JpaRepository<Level, Integer> {
    @Query("""
                    SELECT DISTINCT l
                    FROM Level l
                    LEFT JOIN FETCH l.items i
                    LEFT JOIN FETCH i.itemSlot
                    LEFT JOIN FETCH i.rarity
                    ORDER BY l.level ASC
            """)
    List<Level> findAllWithItemsByOrderByLevelAsc();

}