package pl.gamilife.task.application.edithabit;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.entity.Habit;
import pl.gamilife.task.entity.Task;
import pl.gamilife.task.exception.domain.HabitNotFoundException;
import pl.gamilife.task.repository.HabitRepository;
import pl.gamilife.task.repository.TaskRepository;

import java.util.UUID;

@Component
public class EditHabitUseCaseImpl implements EditHabitUseCase {

    private final HabitRepository habitRepository;
    private final EditHabitMapper editHabitMapper;
    private final TaskRepository taskRepository;

    public EditHabitUseCaseImpl(HabitRepository habitRepository, EditHabitMapper editHabitMapper, TaskRepository taskRepository) {
        this.habitRepository = habitRepository;
        this.editHabitMapper = editHabitMapper;
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
        habit.setCurrentStreak(request.currentStreak());
        habit.setLongestStreak(request.longestStreak()); //todo: business logic


        habit.setAcceptedDate(request.acceptedDate());

        return editHabitMapper.toResponse(habitRepository.save(habit));
    }
}
