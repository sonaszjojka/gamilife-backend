package pl.gamilife.task.infrastructure.persistence;

import org.springframework.stereotype.Repository;
import pl.gamilife.task.domain.model.TaskDifficulty;
import pl.gamilife.task.domain.port.repository.TaskDifficultyRepository;
import pl.gamilife.task.infrastructure.persistence.jpa.JpaTaskDifficultyRepository;

import java.util.Optional;

@Repository
public class TaskDifficultyRepositoryAdapter implements TaskDifficultyRepository {

    private final JpaTaskDifficultyRepository repositoryJpa;

    public TaskDifficultyRepositoryAdapter(JpaTaskDifficultyRepository repositoryJpa) {
        this.repositoryJpa = repositoryJpa;
    }

    @Override
    public Optional<TaskDifficulty> findById(Integer difficultyId) {
        return repositoryJpa.findById(difficultyId);
    }
}
