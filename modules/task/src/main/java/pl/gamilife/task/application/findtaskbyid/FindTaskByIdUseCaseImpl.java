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
        return new FindTaskByIdResult(
                task.getId(),
                task.getTitle(),
                task.getDescription(),
                task.getUserId(),
                new FindTaskByIdResult.TaskCategoryDto(task.getCategoryId(), task.getCategory().getName()),
                new FindTaskByIdResult.TaskDifficultyDto(task.getDifficultyId(), task.getDifficulty().getName()),
                task.getDeadlineDate(),
                task.getDeadlineTime(),
                task.getCompletedAt(),
                null // TODO: habit removal
        );
    }
}
