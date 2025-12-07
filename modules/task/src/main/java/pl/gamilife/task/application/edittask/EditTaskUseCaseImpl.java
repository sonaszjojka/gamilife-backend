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
import pl.gamilife.task.exception.domain.InvalidTaskDataException;
import pl.gamilife.task.exception.domain.TaskCategoryNotFoundException;
import pl.gamilife.task.exception.domain.TaskDifficultyNotFoundException;
import pl.gamilife.task.repository.TaskCategoryRepository;
import pl.gamilife.task.repository.TaskDifficultyRepository;
import pl.gamilife.task.repository.TaskRepository;

import java.time.LocalDateTime;
import java.util.Objects;
import java.util.UUID;

@Component
@AllArgsConstructor
public class EditTaskUseCaseImpl implements EditTaskUseCase {

    private final TaskRepository taskRepository;
    private final EditTaskMapper editTaskMapper;
    private final TaskDifficultyRepository taskDifficultyRepository;
    private final TaskCategoryRepository taskCategoryRepository;
    private final AuthApi currentUserProvider;

    @Override
    @Transactional
    public EditTaskResponse execute(EditTaskRequest request, UUID taskId) {
        Task task = taskRepository.findById(taskId)
                .orElseThrow(() -> new TaskNotFoundException("Task with id " + taskId + " not found."));

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser();
        if (!task.getIsGroupTask() &&!currentUserDto.userId().equals(task.getUserId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not authorized to edit task for another user!");
        }

        if (request.endTime() != null && request.startTime().isAfter(request.endTime())) {
            throw new InvalidTaskDataException("Start time cannot be after end time!");
        }
        if (request.endTime() != null && request.endTime().isBefore(LocalDateTime.now())) {
            throw new InvalidTaskDataException("End time cannot be before creation date");
        }


        task.setTitle(request.title());
        task.setStartTime(request.startTime());
        task.setEndTime(request.endTime());
        task.setCompletedAt(request.completedAt());
        task.setDescription(request.description());

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


        return editTaskMapper.toResponse(taskRepository.save(task));
    }


}
