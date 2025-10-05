package edu.pjwstk.tasks.deletetask;

import edu.pjwstk.tasks.domain.Task;
import edu.pjwstk.tasks.exception.HabitNotFoundException;
import edu.pjwstk.tasks.exception.TaskNotFoundException;
import edu.pjwstk.tasks.repository.HabitRepository;
import edu.pjwstk.tasks.repository.TaskRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Component
public class DeleteTaskUseCaseImpl implements DeleteTaskUseCase {
    private final TaskRepository taskRepository;

    public DeleteTaskUseCaseImpl(TaskRepository taskRepository) {
        this.taskRepository = taskRepository;
    }

    @Override
    @Transactional
    public void execute(UUID taskId) {
        Task task = taskRepository
                .findById(taskId)
                .orElseThrow(() -> new TaskNotFoundException(
                        "Task with id " + taskId + " not found!"
                ));

        taskRepository.deleteById(taskId);
    }
}
