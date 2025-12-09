package pl.gamilife.task.application.deletehabit;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.task.domain.exception.domain.HabitNotFoundException;
import pl.gamilife.task.domain.model.Habit;
import pl.gamilife.task.domain.port.repository.HabitRepository;

@Component
@AllArgsConstructor
public class DeleteHabitUseCaseImpl implements DeleteHabitUseCase {

    private final HabitRepository habitRepository;

    @Override
    @Transactional
    public Void execute(DeleteHabitCommand cmd) {
        Habit habit = habitRepository
                .findHabitByTaskId(cmd.taskId())
                .orElseThrow(() -> new HabitNotFoundException(
                        "Habit for taskId " + cmd.taskId() + " not found!"
                ));

        habitRepository.delete(habit);

        return null;
    }
}
