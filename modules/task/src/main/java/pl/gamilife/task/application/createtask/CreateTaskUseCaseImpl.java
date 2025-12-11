package pl.gamilife.task.application.createtask;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.task.domain.exception.domain.TaskCategoryNotFoundException;
import pl.gamilife.task.domain.exception.domain.TaskDifficultyNotFoundException;
import pl.gamilife.task.domain.model.Task;
import pl.gamilife.task.domain.model.TaskCategory;
import pl.gamilife.task.domain.model.TaskDifficulty;
import pl.gamilife.task.domain.port.repository.TaskCategoryRepository;
import pl.gamilife.task.domain.port.repository.TaskDifficultyRepository;
import pl.gamilife.task.domain.port.repository.TaskRepository;

@Service
@AllArgsConstructor
public class CreateTaskUseCaseImpl implements CreateTaskUseCase {

    private final TaskRepository taskRepository;
    private final TaskCategoryRepository taskCategoryRepository;
    private final TaskDifficultyRepository taskDifficultyRepository;

    @Override
    @Transactional
    public CreateTaskResult execute(CreateTaskCommand cmd) {
        TaskCategory taskCategory = taskCategoryRepository
                .findById(cmd.categoryId())
                .orElseThrow(() -> new TaskCategoryNotFoundException(
                        "Category with id " + cmd.categoryId() + " not found!"
                ));

        TaskDifficulty taskDifficulty = taskDifficultyRepository
                .findById(cmd.difficultyId())
                .orElseThrow(() -> new TaskDifficultyNotFoundException(
                        "Task difficulty with id " + cmd.difficultyId() + " not found!"
                ));

        Task task = Task.create(
                cmd.title(),
                cmd.description(),
                cmd.userId(),
                taskCategory,
                taskDifficulty,
                cmd.deadline()
        );

        taskRepository.save(task);

        return buildResponse(task);
    }

    private CreateTaskResult buildResponse(Task task) {
        return CreateTaskResult.builder()
                .taskId(task.getId())
                .title(task.getTitle())
                .deadline(task.getDeadline())
                .categoryId(task.getCategory() != null ? task.getCategory().getId() : null)
                .difficultyId(task.getDifficulty() != null ? task.getDifficulty().getId() : null)
                .userId(task.getUserId())
                .description(task.getDescription())
                .completedAt(task.getCompletedAt())
                .build();
    }
}
