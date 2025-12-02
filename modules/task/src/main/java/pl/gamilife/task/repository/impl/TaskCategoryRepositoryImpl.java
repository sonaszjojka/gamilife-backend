package pl.gamilife.task.repository.impl;

import pl.gamilife.task.entity.TaskCategory;
import pl.gamilife.task.repository.TaskCategoryRepository;
import pl.gamilife.task.repository.jpa.TaskCategoryRepositoryJpa;
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
