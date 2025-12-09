package pl.gamilife.task.application.edittaskforgrouptask;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
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

@Service
@AllArgsConstructor
public class EditTaskForGroupTaskUseCaseImpl implements EditTaskForGroupTaskUseCase {

    private final TaskRepository taskRepository;
    private final TaskCategoryRepository taskCategoryRepository;
    private final TaskDifficultyRepository taskDifficultyRepository;

    @Override
    public EditTaskForGroupTaskResult execute(EditTaskForGroupTaskCommand cmd) {
        Task task = taskRepository.findById(cmd.taskId()).orElseThrow(
                () -> new TaskNotFoundException("Task with id " + cmd.taskId() + " not found."));

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

        if (!Objects.equals(task.getCategory().getId(), cmd.categoryId())) {
            TaskCategory taskCategory = taskCategoryRepository
                    .findById(cmd.categoryId())
                    .orElseThrow(() -> new TaskCategoryNotFoundException("Category with id " + cmd.categoryId() + " not found!"));
            task.setCategory(taskCategory);
        }

        if (!Objects.equals(task.getDifficulty().getId(), cmd.difficultyId())) {
            TaskDifficulty taskDifficulty = taskDifficultyRepository
                    .findById(cmd.difficultyId())
                    .orElseThrow(() -> new TaskDifficultyNotFoundException("Task difficulty with id " + cmd.difficultyId() + " not found!"));
            task.setDifficulty(taskDifficulty);
        }

        return buildResponse(task);
    }

    private EditTaskForGroupTaskResult buildResponse(Task task) {
        return EditTaskForGroupTaskResult.builder()
                .taskId(task.getId())
                .title(task.getTitle())
                .deadline(task.getDeadline())
                .categoryId(task.getCategory() != null ? task.getCategory().getId() : null)
                .difficultyId(task.getDifficulty() != null ? task.getDifficulty().getId() : null)
                .description(task.getDescription())
                .build();
    }
}
