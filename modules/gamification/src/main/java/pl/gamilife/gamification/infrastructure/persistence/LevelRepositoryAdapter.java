package pl.gamilife.gamification.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Repository;
import pl.gamilife.gamification.domain.model.Level;
import pl.gamilife.gamification.domain.port.repository.LevelRepository;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaLevelRepository;

import java.util.List;
import java.util.Optional;

@Repository
@AllArgsConstructor
public class LevelRepositoryAdapter implements LevelRepository {

    private final JpaLevelRepository jpaLevelRepository;

    @Override
    public List<Level> findAllWithItemsByOrderByLevelAsc() {
        return jpaLevelRepository.findAllWithItemsByOrderByLevelAsc();
    }

    @Override
    public List<Level> findLevelsGained(int level, int experience) {
        return jpaLevelRepository.findLevelsGained(level, experience);
    }

    @Override
    public List<Level> findLevelAfterTutorial() {
        return jpaLevelRepository.findOneLevelBasedOnPageable(PageRequest.of(1, 1));
    }

    @Override
    public Optional<Level> findByLevel(Integer level) {
        return jpaLevelRepository.findById(level);
    }
}
