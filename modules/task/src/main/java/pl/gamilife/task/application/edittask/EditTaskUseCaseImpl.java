package pl.gamilife.task.application.edittask;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.entity.Task;
import pl.gamilife.task.entity.TaskCategory;
import pl.gamilife.task.entity.TaskDifficulty;
import pl.gamilife.task.exception.domain.TaskCategoryNotFoundException;
import pl.gamilife.task.exception.domain.TaskDifficultyNotFoundException;
import pl.gamilife.task.repository.TaskCategoryRepository;
import pl.gamilife.task.repository.TaskDifficultyRepository;
import pl.gamilife.task.repository.TaskRepository;

import java.util.Objects;
import java.util.UUID;

@Component
@AllArgsConstructor
public class EditTaskUseCaseImpl implements EditTaskUseCase {

    private final TaskRepository taskRepository;
    private final TaskDifficultyRepository taskDifficultyRepository;
    private final TaskCategoryRepository taskCategoryRepository;
    private final AuthApi currentUserProvider;

    @Override
    @Transactional
    public EditTaskResponse execute(EditTaskRequest request, UUID taskId) {
        Task task = taskRepository.findById(taskId)
                .orElseThrow(() -> new TaskNotFoundException("Task with id " + taskId + " not found."));

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser();
        if (!task.isGroupTask() && !currentUserDto.userId().equals(task.getUserId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not authorized to edit task for another user!");
        }

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

        if (request.categoryId() != null && !Objects.equals(task.getCategoryId(), request.categoryId())) {
            TaskCategory taskCategory = taskCategoryRepository
                    .findById(request.categoryId())
                    .orElseThrow(() -> new TaskCategoryNotFoundException("Category with id " + request.categoryId() + " not found!"));
            task.setCategory(taskCategory);
        }

        if (request.difficultyId() != null && !Objects.equals(task.getDifficultyId(), request.difficultyId())) {
            TaskDifficulty taskDifficulty = taskDifficultyRepository
                    .findById(request.difficultyId())
                    .orElseThrow(() -> new TaskDifficultyNotFoundException("Task difficulty with id " + request.difficultyId() + " not found!"));
            task.setDifficulty(taskDifficulty);
        }

        return buildResponse(taskRepository.save(task));
    }

    public EditTaskResponse buildResponse(Task task) {
        return EditTaskResponse.builder()
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
