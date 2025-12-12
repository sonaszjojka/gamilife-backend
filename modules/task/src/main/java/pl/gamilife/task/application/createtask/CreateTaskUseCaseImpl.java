package pl.gamilife.task.application.createtask;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.task.domain.exception.domain.TaskCategoryNotFoundException;
import pl.gamilife.task.domain.exception.domain.TaskDifficultyNotFoundException;
import pl.gamilife.task.domain.model.Task;
import pl.gamilife.task.domain.model.TaskCategory;
import pl.gamilife.task.domain.model.TaskDifficulty;
import pl.gamilife.task.domain.port.context.UserContext;
import pl.gamilife.task.domain.port.repository.TaskCategoryRepository;
import pl.gamilife.task.domain.port.repository.TaskDifficultyRepository;
import pl.gamilife.task.domain.port.repository.TaskRepository;

@Service
@AllArgsConstructor
public class CreateTaskUseCaseImpl implements CreateTaskUseCase {

    private final UserContext userContext;
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

        Task task = Task.createPrivate(
                cmd.title(),
                cmd.description(),
                cmd.userId(),
                taskCategory,
                taskDifficulty,
                cmd.deadlineDate(),
                cmd.deadlineTime(),
                userContext.getCurrentUserDateTime(cmd.userId())
        );

        taskRepository.save(task);

        return buildResponse(task);
    }

    private CreateTaskResult buildResponse(Task task) {
        return new CreateTaskResult(
                task.getId(),
                task.getTitle(),
                task.getDescription(),
                task.getDeadlineDate(),
                task.getDeadlineTime(),
                task.getCategory().getId(),
                task.getDifficulty().getId(),
                task.getUserId(),
                task.getCompletedAt()
        );
    }
}
