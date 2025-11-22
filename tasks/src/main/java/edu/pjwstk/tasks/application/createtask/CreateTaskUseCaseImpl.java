package edu.pjwstk.tasks.application.createtask;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.tasks.entity.Habit;
import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.entity.TaskCategory;
import edu.pjwstk.tasks.entity.TaskDifficulty;
import edu.pjwstk.tasks.exception.*;
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
    private final HabitRepository habitRepository;
    private final CreateTaskMapper createTaskMapper;
    private final AuthApi currentUserProvider;

    public CreateTaskUseCaseImpl(TaskRepositoryImpl taskRepository, TaskCategoryRepository taskCategoryRepository, TaskDifficultyRepository taskDifficultyRepository, HabitRepository habitRepository, CreateTaskMapper createTaskMapper, AuthApi currentUserProvider) {
        this.taskRepository = taskRepository;
        this.taskCategoryRepository = taskCategoryRepository;
        this.taskDifficultyRepository = taskDifficultyRepository;
        this.habitRepository = habitRepository;
        this.createTaskMapper = createTaskMapper;
        this.currentUserProvider = currentUserProvider;
    }

    @Override
    @Transactional
    public CreateTaskResponse execute(CreateTaskRequest request) {

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser().orElseThrow();

        if (request.endTime().isBefore(LocalDateTime.now())) {
            throw new InvalidTaskDataException("End date cannot be before creation date");
        }

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

        Habit habit = null;
        if (request.habitTaskId() != null) {
            habit = habitRepository
                    .findById(request.habitTaskId())
                    .orElseThrow(() -> new HabitNotFoundException(
                            "Habit with id " + request.habitTaskId() + " not found!"
                    ));
        }


        Task savedTask = taskRepository.save(
                createTaskMapper.toEntity(
                        request,
                        UUID.randomUUID(),
                        taskCategory,
                        taskDifficulty,
                        habit,
                        currentUserDto.userId()
                )
        );

        return createTaskMapper.toResponse(savedTask);
    }
}
