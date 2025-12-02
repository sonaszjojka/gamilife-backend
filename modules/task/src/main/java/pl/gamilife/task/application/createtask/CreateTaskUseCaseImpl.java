package pl.gamilife.task.application.createtask;

import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.task.entity.Task;
import pl.gamilife.task.entity.TaskCategory;
import pl.gamilife.task.entity.TaskDifficulty;
import pl.gamilife.task.exception.domain.InvalidTaskDataException;
import pl.gamilife.task.exception.domain.TaskCategoryNotFoundException;
import pl.gamilife.task.exception.domain.TaskDifficultyNotFoundException;
import pl.gamilife.task.repository.TaskCategoryRepository;
import pl.gamilife.task.repository.TaskDifficultyRepository;
import pl.gamilife.task.repository.TaskRepository;
import pl.gamilife.task.repository.impl.TaskRepositoryImpl;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

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
