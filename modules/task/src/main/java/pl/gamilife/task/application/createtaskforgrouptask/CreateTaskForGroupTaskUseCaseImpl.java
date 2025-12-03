package pl.gamilife.task.application.createtaskforgrouptask;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;
import pl.gamilife.task.entity.Task;
import pl.gamilife.task.entity.TaskCategory;
import pl.gamilife.task.entity.TaskDifficulty;
import pl.gamilife.task.exception.domain.InvalidTaskDataException;
import pl.gamilife.task.exception.domain.TaskCategoryNotFoundException;
import pl.gamilife.task.exception.domain.TaskDifficultyNotFoundException;
import pl.gamilife.task.repository.HabitRepository;
import pl.gamilife.task.repository.TaskCategoryRepository;
import pl.gamilife.task.repository.TaskDifficultyRepository;
import pl.gamilife.task.repository.TaskRepository;
import pl.gamilife.task.repository.impl.TaskRepositoryImpl;

import java.util.UUID;

@Component
public class CreateTaskForGroupTaskUseCaseImpl implements CreateTaskForGroupTaskUseCase {

    private final TaskRepository taskRepository;
    private final TaskCategoryRepository taskCategoryRepository;
    private final TaskDifficultyRepository taskDifficultyRepository;
    private final CreateTaskForGroupTaskMapper createTaskForGroupTaskMapper;

    public CreateTaskForGroupTaskUseCaseImpl(TaskRepositoryImpl taskRepository, TaskCategoryRepository taskCategoryRepository, TaskDifficultyRepository taskDifficultyRepository, HabitRepository habitRepository, CreateTaskForGroupTaskMapper createTaskForGroupTaskMapper) {
        this.taskRepository = taskRepository;
        this.taskCategoryRepository = taskCategoryRepository;
        this.taskDifficultyRepository = taskDifficultyRepository;
        this.createTaskForGroupTaskMapper = createTaskForGroupTaskMapper;
    }

    @Override
    @Transactional
    public TaskForGroupTaskResponseDto execute(TaskForGroupTaskRequestDto request) {
        if (request.startTime().isAfter(request.endTime())) {
            throw new InvalidTaskDataException("End time date cannot be after start time date!");
        }

        if (request.completedAt() != null && request.startTime().isAfter(request.completedAt())) {
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
