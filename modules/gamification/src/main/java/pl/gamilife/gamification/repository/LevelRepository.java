package pl.gamilife.gamification.repository;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import pl.gamilife.gamification.model.Level;

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

    @Query("""
                    SELECT l
                    FROM Level l
                    ORDER BY l.level ASC
            """)
    List<Level> findStartingLevel(Pageable pageable);

    @Query("""
            SELECT DISTINCT l
            FROM Level l
            LEFT JOIN FETCH l.items i
            WHERE l.level > :currentLevel
              AND l.requiredExperience <= :experience
            ORDER BY l.level ASC
            """)
    List<Level> findLevelsGained(@Param("currentLevel") int currentLevel, @Param("experience") int experience);
}