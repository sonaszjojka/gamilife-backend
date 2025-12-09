package pl.gamilife.task.application.deletehabit;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.entity.Habit;
import pl.gamilife.task.entity.Task;
import pl.gamilife.task.exception.domain.HabitNotFoundException;
import pl.gamilife.task.repository.HabitRepository;
import pl.gamilife.task.repository.TaskRepository;

import java.util.Optional;
import java.util.UUID;

@Component
@AllArgsConstructor
public class DeleteHabitUseCaseImpl implements DeleteHabitUseCase {

    private final HabitRepository habitRepository;

    @Override
    @Transactional
    public void execute(UUID taskId) {
        Habit habit = habitRepository
                .findHabitByTaskId(taskId)
                .orElseThrow(() -> new HabitNotFoundException(
                        "Habit for taskId " + taskId + " not found!"
                ));

        habitRepository.delete(habit);
    }
}
