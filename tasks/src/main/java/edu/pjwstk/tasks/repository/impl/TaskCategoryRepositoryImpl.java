package edu.pjwstk.tasks.repository.impl;

import edu.pjwstk.tasks.domain.TaskCategory;
import edu.pjwstk.tasks.repository.TaskCategoryRepository;
import edu.pjwstk.tasks.repository.jpa.TaskCategoryRepositoryJpa;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class TaskCategoryRepositoryImpl implements TaskCategoryRepository {

    private final TaskCategoryRepositoryJpa repositoryJpa;

    public TaskCategoryRepositoryImpl(TaskCategoryRepositoryJpa repositoryJpa) {
        this.repositoryJpa = repositoryJpa;
    }

    @Override
    public Optional<TaskCategory> findById(Integer categoryId) {
        return repositoryJpa.findById(categoryId);
    }
}
