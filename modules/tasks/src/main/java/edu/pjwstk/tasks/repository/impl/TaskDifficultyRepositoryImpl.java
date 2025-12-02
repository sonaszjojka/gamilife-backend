package edu.pjwstk.tasks.repository.impl;

import edu.pjwstk.tasks.entity.TaskDifficulty;
import edu.pjwstk.tasks.repository.TaskDifficultyRepository;
import edu.pjwstk.tasks.repository.jpa.TaskDifficultyRepositoryJpa;
import org.springframework.stereotype.Repository;

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
