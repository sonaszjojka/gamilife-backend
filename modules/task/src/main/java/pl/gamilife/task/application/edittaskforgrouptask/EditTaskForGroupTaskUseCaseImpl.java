package pl.gamilife.task.application.edittaskforgrouptask;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;
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
import java.util.UUID;

@Service
@AllArgsConstructor
public class EditTaskForGroupTaskUseCaseImpl implements EditTaskForGroupTaskUseCase {

    private final TaskRepository taskRepository;
    private final TaskCategoryRepository taskCategoryRepository;
    private final TaskDifficultyRepository taskDifficultyRepository;

    @Override
    public TaskForGroupTaskResponseDto execute(TaskForGroupTaskRequestDto request, UUID taskId) {
        Task task = taskRepository.findById(taskId).orElseThrow(
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
