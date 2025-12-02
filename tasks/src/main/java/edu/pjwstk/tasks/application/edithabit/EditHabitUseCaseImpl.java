package edu.pjwstk.tasks.application.edithabit;

import pl.gamilife.infrastructure.core.exception.common.domain.TaskNotFoundException;
import edu.pjwstk.tasks.entity.Habit;
import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.exception.domain.HabitNotFoundException;
import edu.pjwstk.tasks.repository.HabitRepository;
import edu.pjwstk.tasks.repository.TaskRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

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
        Task task = taskRepository.findById(taskId).orElseThrow(()-> new TaskNotFoundException(
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
