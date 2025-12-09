package pl.gamilife.task.application.edittaskforgrouptask;

import org.springframework.stereotype.Service;
import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.entity.Task;
import pl.gamilife.task.entity.TaskCategory;
import pl.gamilife.task.entity.TaskDifficulty;
import pl.gamilife.task.exception.domain.TaskCategoryNotFoundException;
import pl.gamilife.task.exception.domain.TaskDifficultyNotFoundException;
import pl.gamilife.task.repository.TaskCategoryRepository;
import pl.gamilife.task.repository.TaskDifficultyRepository;
import pl.gamilife.task.repository.jpa.TaskRepositoryJpa;

import java.util.Objects;
import java.util.UUID;

@Service
public class EditTaskForGroupTaskUseCaseImpl implements EditTaskForGroupTaskUseCase {

    private final TaskRepositoryJpa taskRepositoryJpa;
    private final TaskCategoryRepository taskCategoryRepository;
    private final TaskDifficultyRepository taskDifficultyRepository;

    public EditTaskForGroupTaskUseCaseImpl(TaskRepositoryJpa taskRepositoryJpa, TaskCategoryRepository taskCategoryRepository, TaskDifficultyRepository taskDifficultyRepository) {
        this.taskRepositoryJpa = taskRepositoryJpa;
        this.taskCategoryRepository = taskCategoryRepository;
        this.taskDifficultyRepository = taskDifficultyRepository;
    }

    @Override
    public TaskForGroupTaskResponseDto execute(TaskForGroupTaskRequestDto request, UUID taskId) {
        Task task = taskRepositoryJpa.findById(taskId).orElseThrow(
                () -> new TaskNotFoundException("Task with id " + taskId + " not found."));

        if (request.title() != null) {
            task.setTitle(request.title());
        }

        if (request.deadline() != null) {
            task.setDeadline(request.deadline());
        }

        if (request.description() != null) {
            task.setDescription(request.description());
        }

        if (request.completed()) {
            task.complete();
        }

        if (!Objects.equals(task.getCategory().getId(), request.categoryId())) {
            TaskCategory taskCategory = taskCategoryRepository
                    .findById(request.categoryId())
                    .orElseThrow(() -> new TaskCategoryNotFoundException("Category with id " + request.categoryId() + " not found!"));
            task.setCategory(taskCategory);
        }

        if (!Objects.equals(task.getDifficulty().getId(), request.difficultyId())) {
            TaskDifficulty taskDifficulty = taskDifficultyRepository
                    .findById(request.difficultyId())
                    .orElseThrow(() -> new TaskDifficultyNotFoundException("Task difficulty with id " + request.difficultyId() + " not found!"));
            task.setDifficulty(taskDifficulty);
        }

        return buildResponse(task);
    }

    private TaskForGroupTaskResponseDto buildResponse(Task task) {
        return TaskForGroupTaskResponseDto.builder()
                .taskId(task.getId())
                .title(task.getTitle())
                .deadline(task.getDeadline())
                .categoryId(task.getCategory() != null ? task.getCategory().getId() : null)
                .difficultyId(task.getDifficulty() != null ? task.getDifficulty().getId() : null)
                .description(task.getDescription())
                .build();
    }
}
