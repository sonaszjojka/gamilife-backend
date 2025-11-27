package edu.pjwstk.tasks.application.edittask;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.exception.common.domain.ResourceOwnerPrivilegesRequiredException;
import edu.pjwstk.core.exception.common.domain.TaskNotFoundException;
import edu.pjwstk.tasks.entity.Habit;
import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.entity.TaskCategory;
import edu.pjwstk.tasks.entity.TaskDifficulty;
import edu.pjwstk.tasks.exception.domain.HabitNotFoundException;
import edu.pjwstk.tasks.exception.domain.InvalidTaskDataException;
import edu.pjwstk.tasks.exception.domain.TaskCategoryNotFoundException;
import edu.pjwstk.tasks.exception.domain.TaskDifficultyNotFoundException;
import edu.pjwstk.tasks.repository.HabitRepository;
import edu.pjwstk.tasks.repository.TaskCategoryRepository;
import edu.pjwstk.tasks.repository.TaskDifficultyRepository;
import edu.pjwstk.tasks.repository.TaskRepository;
import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

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
    private final HabitRepository habitRepository;
    private final AuthApi currentUserProvider;
    private final ApplicationEventPublisher eventPublisher;

    @Override
    @Transactional
    public EditTaskResponse execute(EditTaskRequest request, UUID taskId) {
        Task task = taskRepository.findById(taskId)
                .orElseThrow(() -> new TaskNotFoundException("Task with id " + taskId + " not found."));

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser();
        if (!currentUserDto.userId().equals(task.getUserId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not authorized to edit task for another user!");
        }

        if (request.endTime() != null && request.startTime().isAfter(request.endTime())) {
            throw new InvalidTaskDataException("Start time cannot be after end time!");
        }
        if (request.endTime()!=null&& request.endTime().isBefore(LocalDateTime.now())) {
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
