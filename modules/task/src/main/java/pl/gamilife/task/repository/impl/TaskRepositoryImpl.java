package pl.gamilife.task.repository.impl;

import org.springframework.stereotype.Repository;
import pl.gamilife.task.entity.Task;
import pl.gamilife.task.repository.TaskRepository;
import pl.gamilife.task.repository.jpa.TaskRepositoryJpa;

import java.util.Optional;
import java.util.UUID;

@Repository
public class TaskRepositoryImpl implements TaskRepository {

    private final TaskRepositoryJpa repositoryJpa;

    public TaskRepositoryImpl(TaskRepositoryJpa repositoryJpa) {
        this.repositoryJpa = repositoryJpa;
    }

    @Override
    public Task save(Task task) {
        return repositoryJpa.save(task);
    }

    @Override
    public Optional<Task> findById(UUID taskId) {
        return repositoryJpa.findById(taskId);
    }

    @Override
    public void deleteById(UUID taskId) {
        repositoryJpa.deleteById(taskId);
    }

    @Override
    public Boolean existsById(UUID taskId) {
        return repositoryJpa.existsById(taskId);
    }


}
