package pl.gamilife.task.application.findtaskbyid;

import org.springframework.stereotype.Component;
import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.domain.model.Task;
import pl.gamilife.task.domain.port.repository.TaskRepository;

import java.util.UUID;

@Component
public class FindTaskByIdUseCaseImpl implements FindTaskByIdUseCase {

    private final TaskRepository taskRepository;

    public FindTaskByIdUseCaseImpl(TaskRepository taskRepository) {
        this.taskRepository = taskRepository;
    }

    @Override
    public TaskDto execute(UUID taskId) {
        Task task = taskRepository.findById(taskId)
                .orElseThrow(() -> new TaskNotFoundException("Task with id: " + taskId + " not found!"));
        return buildResponse(task);
    }

    private TaskDto buildResponse(Task task) {
        return TaskDto.builder()
                .id(task.getId())
                .title(task.getTitle())
                .deadline(task.getDeadline())
                .category(
                        task.getCategory() != null
                                ? new TaskDto.TaskCategoryDto(task.getCategory().getId(), task.getCategory().getName())
                                : null
                )
                .difficulty(
                        task.getDifficulty() != null
                                ? new TaskDto.TaskDifficultyDto(task.getDifficulty().getId(), task.getDifficulty().getName())
                                : null
                )
                .userId(task.getUserId())
                .completedAt(task.getCompletedAt())
                .description(task.getDescription())
                .build();
    }
}
