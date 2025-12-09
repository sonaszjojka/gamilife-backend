package pl.gamilife.task.application.edithabit;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.controllers.request.EditHabitRequest;
import pl.gamilife.task.controllers.response.EditHabitResponse;
import pl.gamilife.task.entity.Habit;
import pl.gamilife.task.entity.Task;
import pl.gamilife.task.exception.domain.HabitNotFoundException;
import pl.gamilife.task.repository.HabitRepository;
import pl.gamilife.task.repository.TaskRepository;

import java.util.UUID;

@Component
public class EditHabitUseCaseImpl implements EditHabitUseCase {

    private final HabitRepository habitRepository;
    private final TaskRepository taskRepository;

    public EditHabitUseCaseImpl(HabitRepository habitRepository, TaskRepository taskRepository) {
        this.habitRepository = habitRepository;
        this.taskRepository = taskRepository;
    }

    @Override
    @Transactional
    public EditHabitResponse execute(EditHabitRequest request, UUID habitId, UUID taskId) {

        Habit habit = habitRepository.findById(habitId)
                .orElseThrow(() -> new HabitNotFoundException(
                        "Habit with id " + habitId + " not found!"
                ));
        Task task = taskRepository.findById(taskId).orElseThrow(() -> new TaskNotFoundException(
                "Task with id " + taskId + " not found!"
        ));

        if (!habit.getTask().getId().equals(task.getId())) {
            throw new HabitNotFoundException(
                    "Habit with id " + habitId + " not found for task with id " + taskId
            );
        }

        habit.setCycleLength(request.cycleLength());

        if (request.finished()) {
            habit.finish();
        }

        return buildResponse(habitRepository.save(habit));
    }

    public EditHabitResponse buildResponse(Habit habit) {
        return new EditHabitResponse(
                habit.getId(),
                habit.getCycleLength(),
                habit.getCurrentStreak(),
                habit.getLongestStreak(),
                habit.getFinishedAt(),
                habit.getUpdatedAt(),
                habit.getCreatedAt()
        );
    }
}
