package edu.pjwstk.tasks.application.createtask;

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
import edu.pjwstk.tasks.repository.impl.TaskRepositoryImpl;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.UUID;

@Component
public class CreateTaskUseCaseImpl implements CreateTaskUseCase {

    private final TaskRepository taskRepository;
    private final TaskCategoryRepository taskCategoryRepository;
    private final TaskDifficultyRepository taskDifficultyRepository;
    private final CreateTaskMapper createTaskMapper;
    private final AuthApi currentUserProvider;

    public CreateTaskUseCaseImpl(TaskRepositoryImpl taskRepository, TaskCategoryRepository taskCategoryRepository, TaskDifficultyRepository taskDifficultyRepository, CreateTaskMapper createTaskMapper, AuthApi currentUserProvider) {
        this.taskRepository = taskRepository;
        this.taskCategoryRepository = taskCategoryRepository;
        this.taskDifficultyRepository = taskDifficultyRepository;
        this.createTaskMapper = createTaskMapper;
        this.currentUserProvider = currentUserProvider;
    }

    @Override
    @Transactional
    public CreateTaskResponse execute(CreateTaskRequest request) {

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser();

        if (request.startTime().isAfter(request.endTime())) {
            throw new InvalidTaskDataException("End time date cannot be after start time date!");
        }

        if (request.completedAt()!=null&&request.startTime().isAfter(request.completedAt())) {
            throw new InvalidTaskDataException("Completed at date cannot be after start time date!");
        }

        TaskCategory taskCategory = taskCategoryRepository
                .findById(request.categoryId())
                .orElseThrow(() -> new TaskCategoryNotFoundException(
                        "Category with id " + request.categoryId() + " not found!"
                ));

        TaskDifficulty taskDifficulty = taskDifficultyRepository
                .findById(request.difficultyId())
                .orElseThrow(() -> new TaskDifficultyNotFoundException(
                        "Task difficulty with id " + request.difficultyId() + " not found!"
                ));



        Task savedTask = taskRepository.save(
                createTaskMapper.toEntity(
                        request,
                        UUID.randomUUID(),
                        taskCategory,
                        taskDifficulty,
                        currentUserDto.userId()
                )
        );

        return createTaskMapper.toResponse(savedTask);
    }
}
