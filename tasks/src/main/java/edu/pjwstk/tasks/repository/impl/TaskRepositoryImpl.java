package edu.pjwstk.tasks.repository.impl;

import edu.pjwstk.tasks.domain.Task;
import edu.pjwstk.tasks.repository.TaskRepository;
import edu.pjwstk.tasks.repository.jpa.TaskRepositoryJpa;
import org.springframework.stereotype.Repository;

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

}
