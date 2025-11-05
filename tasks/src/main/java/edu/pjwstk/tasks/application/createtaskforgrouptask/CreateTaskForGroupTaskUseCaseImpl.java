package edu.pjwstk.tasks.application.createtaskforgrouptask;

import edu.pjwstk.api.tasks.dto.TaskForGroupTaskRequestDto;
import edu.pjwstk.api.tasks.dto.TaskForGroupTaskResponseDto;
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

import java.util.UUID;

@Component
public class CreateTaskForGroupTaskUseCaseImpl implements CreateTaskForGroupTaskUseCase {

    private final TaskRepository taskRepository;
    private final TaskCategoryRepository taskCategoryRepository;
    private final TaskDifficultyRepository taskDifficultyRepository;
    private final HabitRepository habitRepository;
    private final CreateTaskForGroupTaskMapper createTaskForGroupTaskMapper;

    public CreateTaskForGroupTaskUseCaseImpl(TaskRepositoryImpl taskRepository, TaskCategoryRepository taskCategoryRepository, TaskDifficultyRepository taskDifficultyRepository, HabitRepository habitRepository, CreateTaskForGroupTaskMapper createTaskForGroupTaskMapper) {
        this.taskRepository = taskRepository;
        this.taskCategoryRepository = taskCategoryRepository;
        this.taskDifficultyRepository = taskDifficultyRepository;
        this.habitRepository = habitRepository;
        this.createTaskForGroupTaskMapper = createTaskForGroupTaskMapper;
    }

    @Override
    @Transactional
    public TaskForGroupTaskResponseDto execute(TaskForGroupTaskRequestDto request) {
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
                createTaskForGroupTaskMapper.toEntity(
                        request,
                        UUID.randomUUID(),
                        taskCategory,
                        taskDifficulty
                )
        );

        return createTaskForGroupTaskMapper.toResponse(savedTask);
    }
}
