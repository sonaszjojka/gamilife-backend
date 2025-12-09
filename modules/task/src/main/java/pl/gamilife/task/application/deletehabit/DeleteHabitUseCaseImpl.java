package pl.gamilife.task.application.deletehabit;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.task.domain.exception.domain.HabitNotFoundException;
import pl.gamilife.task.domain.model.Habit;
import pl.gamilife.task.domain.port.repository.HabitRepository;

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
