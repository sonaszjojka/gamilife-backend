package pl.gamilife.task.application.createtaskforgrouptask;

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

import java.time.LocalDateTime;

@Service
@AllArgsConstructor
public class CreateTaskForGroupTaskUseCaseImpl implements CreateTaskForGroupTaskUseCase {

    private final TaskRepository taskRepository;
    private final TaskCategoryRepository taskCategoryRepository;
    private final TaskDifficultyRepository taskDifficultyRepository;

    @Override
    @Transactional
    public CreateTaskForGroupTaskResult execute(CreateTaskForGroupTaskCommand cmd) {
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

        Task task = Task.createForGroupTask(
                cmd.title(),
                cmd.description(),
                taskCategory,
                taskDifficulty,
                cmd.deadlineDate(),
                cmd.deadlineTime(),
                LocalDateTime.now(cmd.currentGroupTimezone())
        );
        taskRepository.save(task);

        return buildResponse(task);
    }

    public CreateTaskForGroupTaskResult buildResponse(Task task) {
        return new CreateTaskForGroupTaskResult(
                task.getId(),
                task.getTitle(),
                task.getDeadlineDate(),
                task.getDeadlineTime(),
                task.getCategory().getId(),
                task.getDifficulty().getId(),
                task.getDescription()
        );
    }
}
