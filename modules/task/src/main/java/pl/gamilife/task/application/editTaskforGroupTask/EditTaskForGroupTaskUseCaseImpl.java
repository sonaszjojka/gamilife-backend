package pl.gamilife.task.application.editTaskforGroupTask;

import org.springframework.stereotype.Service;
import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;
import pl.gamilife.infrastructure.core.exception.common.domain.TaskNotFoundException;
import pl.gamilife.task.entity.Task;
import pl.gamilife.task.entity.TaskCategory;
import pl.gamilife.task.entity.TaskDifficulty;
import pl.gamilife.task.exception.domain.TaskCategoryNotFoundException;
import pl.gamilife.task.exception.domain.TaskDifficultyNotFoundException;
import pl.gamilife.task.repository.TaskCategoryRepository;
import pl.gamilife.task.repository.TaskDifficultyRepository;
import pl.gamilife.task.repository.jpa.TaskRepositoryJpa;

import java.util.Objects;
import java.util.UUID;

@Service
public class EditTaskForGroupTaskUseCaseImpl implements EditTaskForGroupTaskUseCase {
    private final TaskRepositoryJpa taskRepositoryJpa;
    private final TaskCategoryRepository taskCategoryRepository;
    private final TaskDifficultyRepository taskDifficultyRepository;
    private final EditTaskForGroupTaskMapper editTaskForGroupTaskMapper;


    public EditTaskForGroupTaskUseCaseImpl(TaskRepositoryJpa taskRepositoryJpa, TaskCategoryRepository taskCategoryRepository, TaskDifficultyRepository taskDifficultyRepository, EditTaskForGroupTaskMapper editTaskForGroupTaskMapper) {
        this.taskRepositoryJpa = taskRepositoryJpa;
        this.taskCategoryRepository = taskCategoryRepository;
        this.taskDifficultyRepository = taskDifficultyRepository;
        this.editTaskForGroupTaskMapper = editTaskForGroupTaskMapper;
    }



    @Override
    public TaskForGroupTaskResponseDto execute(TaskForGroupTaskRequestDto request, UUID taskId) {

        Task task = taskRepositoryJpa.findById(taskId).orElseThrow(
                () -> new TaskNotFoundException("Task with id " + taskId + " not found."));

        task.setStartTime(request.startTime());
        task.setEndTime(request.endTime());
        task.setDescription(request.description());
        task.setTitle(request.title());
        task.setCompletedAt(request.completedAt());


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



        return editTaskForGroupTaskMapper.toResponse(task);
    }
}
