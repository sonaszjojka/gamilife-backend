package pl.gamilife.task.repository.impl;

import org.springframework.stereotype.Repository;
import pl.gamilife.task.entity.TaskDifficulty;
import pl.gamilife.task.repository.TaskDifficultyRepository;
import pl.gamilife.task.repository.jpa.TaskDifficultyRepositoryJpa;

import java.util.Optional;

@Repository
public class TaskDifficultyRepositoryImpl implements TaskDifficultyRepository {

    private final TaskDifficultyRepositoryJpa repositoryJpa;

    public TaskDifficultyRepositoryImpl(TaskDifficultyRepositoryJpa repositoryJpa) {
        this.repositoryJpa = repositoryJpa;
    }

    @Override
    public Optional<TaskDifficulty> findById(Integer difficultyId) {
        return repositoryJpa.findById(difficultyId);
    }
}
