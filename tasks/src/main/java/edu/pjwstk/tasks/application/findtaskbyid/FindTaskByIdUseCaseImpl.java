package edu.pjwstk.tasks.application.findtaskbyid;

import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.exception.TaskNotFoundException;
import edu.pjwstk.tasks.repository.TaskRepository;
import org.springframework.stereotype.Component;

import java.util.Optional;
import java.util.UUID;

@Component
public class FindTaskByIdUseCaseImpl implements FindTaskByIdUseCase {

    private final TaskRepository taskRepository;

    public FindTaskByIdUseCaseImpl(TaskRepository taskRepository) {
        this.taskRepository = taskRepository;
    }

    @Override
    public Task execute(UUID taskId) {
        return taskRepository.findById(taskId)
                .orElseThrow(() -> new TaskNotFoundException("Task with id: " + taskId + " not found!"));
    }
}
