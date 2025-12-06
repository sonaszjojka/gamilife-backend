package pl.gamilife.gamification.domain.port.repository;

import pl.gamilife.gamification.domain.model.Level;

import java.util.List;

public interface LevelRepository {
    List<Level> findAllWithItemsByOrderByLevelAsc();

    List<Level> findStartingLevel();

    List<Level> findLevelsGained(int level, int experience);

    List<Level> findLevelAfterTutorial();
}
