package pl.gamilife.task.infrastructure.persistence;

import org.springframework.stereotype.Repository;
import pl.gamilife.task.domain.model.TaskCategory;
import pl.gamilife.task.domain.port.repository.TaskCategoryRepository;
import pl.gamilife.task.infrastructure.persistence.jpa.JpaTaskCategoryRepository;

import java.util.Optional;

@Repository
public class TaskCategoryRepositoryAdapter implements TaskCategoryRepository {

    private final JpaTaskCategoryRepository repositoryJpa;

    public TaskCategoryRepositoryAdapter(JpaTaskCategoryRepository repositoryJpa) {
        this.repositoryJpa = repositoryJpa;
    }

    @Override
    public Optional<TaskCategory> findById(Integer categoryId) {
        return repositoryJpa.findById(categoryId);
    }
}
