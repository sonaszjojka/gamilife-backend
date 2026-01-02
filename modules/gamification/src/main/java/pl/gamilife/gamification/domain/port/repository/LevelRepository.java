package pl.gamilife.gamification.domain.port.repository;

import pl.gamilife.gamification.domain.model.Level;

import java.util.List;
import java.util.Optional;

public interface LevelRepository {
    List<Level> findAllWithItemsByOrderByLevelAsc();

    List<Level> findLevelsGainedOrderByLevelAsc(int level, int experience);

    List<Level> findLevelAfterTutorial();

    Optional<Level> findByLevel(Integer integer);
}
