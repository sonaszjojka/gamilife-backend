package pl.gamilife.task.application.createtaskforgrouptask;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;
import pl.gamilife.task.domain.exception.domain.TaskCategoryNotFoundException;
import pl.gamilife.task.domain.exception.domain.TaskDifficultyNotFoundException;
import pl.gamilife.task.domain.model.Task;
import pl.gamilife.task.domain.model.TaskCategory;
import pl.gamilife.task.domain.model.TaskDifficulty;
import pl.gamilife.task.domain.port.repository.TaskCategoryRepository;
import pl.gamilife.task.domain.port.repository.TaskDifficultyRepository;
import pl.gamilife.task.domain.port.repository.TaskRepository;
import pl.gamilife.task.infrastructure.persistence.TaskRepositoryAdapter;

@Component
public class CreateTaskForGroupTaskUseCaseImpl implements CreateTaskForGroupTaskUseCase {

    private final TaskRepository taskRepository;
    private final TaskCategoryRepository taskCategoryRepository;
    private final TaskDifficultyRepository taskDifficultyRepository;

    public CreateTaskForGroupTaskUseCaseImpl(TaskRepositoryAdapter taskRepository, TaskCategoryRepository taskCategoryRepository, TaskDifficultyRepository taskDifficultyRepository) {
        this.taskRepository = taskRepository;
        this.taskCategoryRepository = taskCategoryRepository;
        this.taskDifficultyRepository = taskDifficultyRepository;
    }

    @Override
    @Transactional
    public TaskForGroupTaskResponseDto execute(TaskForGroupTaskRequestDto request) {
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

        Task task = Task.builder()
                .title(request.title())
                .deadline(request.deadline())
                .categoryId(taskCategory.getId())
                .difficulty(taskDifficulty)
                .userId(null)
                .description(request.description())
                .build();

        task = taskRepository.save(task);

        return buildResponse(task);
    }

    public TaskForGroupTaskResponseDto buildResponse(Task task) {
        return TaskForGroupTaskResponseDto.builder()
                .taskId(task.getId())
                .title(task.getTitle())
                .deadline(task.getDeadline())
                .categoryId(task.getCategory().getId())
                .difficultyId(task.getDifficulty().getId())
                .description(task.getDescription())
                .build();
    }
}
