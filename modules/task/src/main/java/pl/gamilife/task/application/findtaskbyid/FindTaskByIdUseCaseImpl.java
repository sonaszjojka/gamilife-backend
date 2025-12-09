package pl.gamilife.task.application.findtaskbyid;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.domain.model.Task;
import pl.gamilife.task.domain.port.repository.TaskRepository;

@Service
@AllArgsConstructor
public class FindTaskByIdUseCaseImpl implements FindTaskByIdUseCase {

    private final TaskRepository taskRepository;

    @Override
    public FindTaskByIdResult execute(FindTaskByIdCommand cmd) {
        Task task = taskRepository.findById(cmd.taskId())
                .orElseThrow(() -> new TaskNotFoundException("Task with id: " + cmd.taskId() + " not found!"));
        return buildResponse(task);
    }

    private FindTaskByIdResult buildResponse(Task task) {
        return FindTaskByIdResult.builder()
                .id(task.getId())
                .title(task.getTitle())
                .deadline(task.getDeadline())
                .category(
                        task.getCategory() != null
                                ? new FindTaskByIdResult.TaskCategoryDto(task.getCategory().getId(), task.getCategory().getName())
                                : null
                )
                .difficulty(
                        task.getDifficulty() != null
                                ? new FindTaskByIdResult.TaskDifficultyDto(task.getDifficulty().getId(), task.getDifficulty().getName())
                                : null
                )
                .userId(task.getUserId())
                .completedAt(task.getCompletedAt())
                .description(task.getDescription())
                .build();
    }
}
