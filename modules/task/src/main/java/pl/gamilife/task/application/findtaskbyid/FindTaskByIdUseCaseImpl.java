package pl.gamilife.task.application.findtaskbyid;

import org.springframework.stereotype.Component;
import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.entity.Task;
import pl.gamilife.task.repository.TaskRepository;
import pl.gamilife.task.shared.TaskProviderMapper;

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
