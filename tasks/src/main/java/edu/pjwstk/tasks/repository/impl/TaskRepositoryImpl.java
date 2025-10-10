package edu.pjwstk.tasks.repository.impl;

import edu.pjwstk.tasks.domain.Task;
import edu.pjwstk.tasks.repository.TaskRepository;
import edu.pjwstk.tasks.repository.jpa.TaskRepositoryJpa;
import org.springframework.stereotype.Repository;

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

}
