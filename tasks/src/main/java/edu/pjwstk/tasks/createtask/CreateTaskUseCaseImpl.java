package edu.pjwstk.tasks.createtask;

import edu.pjwstk.tasks.domain.Habit;
import edu.pjwstk.tasks.domain.Task;
import edu.pjwstk.tasks.domain.TaskCategory;
import edu.pjwstk.tasks.domain.TaskDifficulty;
import edu.pjwstk.tasks.exception.*;
import edu.pjwstk.tasks.repository.HabitRepository;
import edu.pjwstk.tasks.repository.TaskCategoryRepository;
import edu.pjwstk.tasks.repository.TaskDifficultyRepository;
import edu.pjwstk.tasks.repository.TaskRepository;
import edu.pjwstk.tasks.repository.impl.TaskRepositoryImpl;
import jdk.jfr.Category;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import java.util.UUID;

@Component
public class CreateTaskUseCaseImpl implements CreateTaskUseCase {

    private final TaskRepository taskRepository;
    private final TaskCategoryRepository taskCategoryRepository;
    private final TaskDifficultyRepository taskDifficultyRepository;
    private final HabitRepository habitRepository;
    private final CreateTaskMapper createTaskMapper;

    public CreateTaskUseCaseImpl(TaskRepositoryImpl taskRepository, TaskCategoryRepository taskCategoryRepository, TaskDifficultyRepository taskDifficultyRepository, HabitRepository habitRepository, CreateTaskMapper createTaskMapper) {
        this.taskRepository = taskRepository;
        this.taskCategoryRepository = taskCategoryRepository;
        this.taskDifficultyRepository = taskDifficultyRepository;
        this.habitRepository = habitRepository;
        this.createTaskMapper = createTaskMapper;
    }

    @Override
    @Transactional
    public CreateTaskResponse execute(CreateTaskRequest request) {
        if (request.startTime().isAfter(request.endTime())) {
            throw new InvalidTaskDataException("End time date cannot be after start time date!");
        }

        if (request.startTime().isAfter(request.completedAt())) {
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

        Task previousTask = null;
        if (request.previousTaskId() != null) {
            previousTask = taskRepository
                    .findById(request.previousTaskId())
                    .orElseThrow(() -> new TaskNotFoundException(
                            "Previous task with id " + request.previousTaskId() + " not found!"
                    ));
        }

        Task savedTask = taskRepository.save(
                createTaskMapper.toEntity(
                        request,
                        UUID.randomUUID(),
                        taskCategory,
                        taskDifficulty,
                        habit,
                        previousTask
                )
        );

        return createTaskMapper.toResponse(savedTask);
    }
}
