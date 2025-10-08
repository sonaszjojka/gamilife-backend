package edu.pjwstk.tasks.edittask;

import edu.pjwstk.tasks.domain.*;
import edu.pjwstk.tasks.exception.*;
import edu.pjwstk.tasks.repository.*;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;
import java.util.UUID;

@Component
public class EditTaskUseCaseImpl implements EditTaskUseCase {

    private final TaskRepository taskRepository;
    private final EditTaskMapper editTaskMapper;
    private final TaskDifficultyRepository taskDifficultyRepository;
    private final TaskCategoryRepository taskCategoryRepository;
    private final HabitRepository habitRepository;

    public EditTaskUseCaseImpl(TaskRepository taskRepository, EditTaskMapper editTaskMapper,
                               TaskDifficultyRepository taskDifficultyRepository,
                               TaskCategoryRepository taskCategoryRepository, HabitRepository habitRepository) {
        this.taskRepository = taskRepository;
        this.editTaskMapper = editTaskMapper;
        this.taskDifficultyRepository = taskDifficultyRepository;
        this.taskCategoryRepository = taskCategoryRepository;
        this.habitRepository = habitRepository;
    }

    @Override
    @Transactional
    public EditTaskResponse execute(EditTaskRequest request, UUID taskId) {
        Task task = taskRepository.findById(taskId)
                .orElseThrow(() -> new TaskNotFoundException("Task with id " + taskId + " not found."));

        if (request.endTime() != null && request.startTime().isAfter(request.endTime())) {
            throw new InvalidTaskDataException("Start time cannot be after end time!");
        }

        if (request.completedAt() != null && request.startTime().isAfter(request.completedAt())) {
            throw new InvalidTaskDataException("Start time cannot be after completion date!");
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

        if (request.habitTaskId() != null) {
            if (task.getHabitTask() == null) {
                Habit habit = habitRepository
                        .findById(request.habitTaskId())
                        .orElseThrow(() -> new HabitNotFoundException(
                                "Habit with id " + request.habitTaskId() + " not found!"
                        ));
                task.setHabitTask(habit);
            } else if(!Objects.equals(task.getHabitTask().getId(), request.habitTaskId())){
                throw new InvalidTaskDataException("Habit task cannot be changed after assigning!");
            }
        }

        if (request.previousTaskId() != null) {
            if (!request.previousTaskId().equals(taskId)) {
                Task previousTask = taskRepository
                        .findById(request.previousTaskId())
                        .orElseThrow(() -> new TaskNotFoundException("Previous task with id " + request.previousTaskId() + " not found!"));
                task.setPreviousTask(previousTask);
            }
        } else {
            task.setPreviousTask(null);
        }

        return editTaskMapper.toResponse(taskRepository.save(task));
    }


}
