package pl.gamilife.task.application.edittask;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.domain.exception.domain.TaskCategoryNotFoundException;
import pl.gamilife.task.domain.exception.domain.TaskDifficultyNotFoundException;
import pl.gamilife.task.domain.model.Task;
import pl.gamilife.task.domain.model.TaskCategory;
import pl.gamilife.task.domain.model.TaskDifficulty;
import pl.gamilife.task.domain.port.repository.TaskCategoryRepository;
import pl.gamilife.task.domain.port.repository.TaskDifficultyRepository;
import pl.gamilife.task.domain.port.repository.TaskRepository;

import java.util.Objects;

@Component
@AllArgsConstructor
public class EditTaskUseCaseImpl implements EditTaskUseCase {

    private final TaskRepository taskRepository;
    private final TaskDifficultyRepository taskDifficultyRepository;
    private final TaskCategoryRepository taskCategoryRepository;

    @Override
    @Transactional
    public EditTaskResult execute(EditTaskCommand cmd) {
        Task task = taskRepository.findById(cmd.taskId())
                .orElseThrow(() -> new TaskNotFoundException("Task with id " + cmd.taskId() + " not found."));

        if (!task.isGroupTask() && !cmd.userId().equals(task.getUserId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not authorized to edit task for another user!");
        }

        if (cmd.title() != null) {
            task.setTitle(cmd.title());
        }

        if (cmd.deadline() != null) {
            task.setDeadline(cmd.deadline());
        }

        if (cmd.description() != null) {
            task.setDescription(cmd.description());
        }

        if (cmd.completed()) {
            task.complete();
        }

        if (cmd.categoryId() != null && !Objects.equals(task.getCategoryId(), cmd.categoryId())) {
            TaskCategory taskCategory = taskCategoryRepository
                    .findById(cmd.categoryId())
                    .orElseThrow(() -> new TaskCategoryNotFoundException("Category with id " + cmd.categoryId() + " not found!"));
            task.setCategory(taskCategory);
        }

        if (cmd.difficultyId() != null && !Objects.equals(task.getDifficultyId(), cmd.difficultyId())) {
            TaskDifficulty taskDifficulty = taskDifficultyRepository
                    .findById(cmd.difficultyId())
                    .orElseThrow(() -> new TaskDifficultyNotFoundException("Task difficulty with id " + cmd.difficultyId() + " not found!"));
            task.setDifficulty(taskDifficulty);
        }

        return buildResponse(taskRepository.save(task));
    }

    public EditTaskResult buildResponse(Task task) {
        return EditTaskResult.builder()
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
