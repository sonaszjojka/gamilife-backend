package pl.gamilife.task.application.createtask;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.task.domain.exception.domain.TaskCategoryNotFoundException;
import pl.gamilife.task.domain.exception.domain.TaskDifficultyNotFoundException;
import pl.gamilife.task.domain.model.Task;
import pl.gamilife.task.domain.model.TaskCategory;
import pl.gamilife.task.domain.model.TaskDifficulty;
import pl.gamilife.task.domain.port.repository.TaskCategoryRepository;
import pl.gamilife.task.domain.port.repository.TaskDifficultyRepository;
import pl.gamilife.task.domain.port.repository.TaskRepository;
import pl.gamilife.task.infrastructure.persistence.TaskRepositoryAdapter;
import pl.gamilife.task.infrastructure.web.request.CreateTaskRequest;
import pl.gamilife.task.infrastructure.web.response.CreateTaskResponse;

@Component
public class CreateTaskUseCaseImpl implements CreateTaskUseCase {

    private final TaskRepository taskRepository;
    private final TaskCategoryRepository taskCategoryRepository;
    private final TaskDifficultyRepository taskDifficultyRepository;
    private final AuthApi currentUserProvider;

    public CreateTaskUseCaseImpl(TaskRepositoryAdapter taskRepository, TaskCategoryRepository taskCategoryRepository, TaskDifficultyRepository taskDifficultyRepository, AuthApi currentUserProvider) {
        this.taskRepository = taskRepository;
        this.taskCategoryRepository = taskCategoryRepository;
        this.taskDifficultyRepository = taskDifficultyRepository;
        this.currentUserProvider = currentUserProvider;
    }

    @Override
    @Transactional
    public CreateTaskResponse execute(CreateTaskRequest request) {
        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser();

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
                .categoryId(request.categoryId())
                .difficultyId(request.difficultyId())
                .userId(currentUserDto.userId())
                .description(request.description())
                .build();

        task = taskRepository.save(task);

        return buildResponse(task);
    }

    private CreateTaskResponse buildResponse(Task task) {
        return CreateTaskResponse.builder()
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
