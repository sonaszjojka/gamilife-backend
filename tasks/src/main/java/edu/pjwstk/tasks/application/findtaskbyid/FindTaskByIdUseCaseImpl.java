package edu.pjwstk.tasks.application.findtaskbyid;

import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.infrastructure.core.exception.common.domain.TaskNotFoundException;
import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.repository.TaskRepository;
import edu.pjwstk.tasks.shared.TaskProviderMapper;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class FindTaskByIdUseCaseImpl implements FindTaskByIdUseCase {

    private final TaskRepository taskRepository;
    private final TaskProviderMapper taskProviderMapper;

    public FindTaskByIdUseCaseImpl(TaskRepository taskRepository, TaskProviderMapper taskProviderMapper) {
        this.taskRepository = taskRepository;
        this.taskProviderMapper = taskProviderMapper;
    }

    @Override
    public TaskDto execute(UUID taskId) {
        Task task = taskRepository.findById(taskId)
                .orElseThrow(() -> new TaskNotFoundException("Task with id: " + taskId + " not found!"));
        return taskProviderMapper.toResponse(task);
    }
}
